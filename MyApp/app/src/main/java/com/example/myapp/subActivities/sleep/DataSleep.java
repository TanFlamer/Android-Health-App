package com.example.myapp.subActivities.sleep;

import android.annotation.SuppressLint;
import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.sleep.Sleep;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Objects;

public class DataSleep extends AppCompatActivity{

    DataSleepViewModel dataSleepViewModel;

    Button buttonDate, sleepTime, wakeTime, buttonSave, buttonReturn;
    TextView durationView;

    int year, month, day;
    int sleepHour, sleepMinute;
    int wakeHour, wakeMinute;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sleep);
        dataSleepViewModel = new ViewModelProvider(this).get(DataSleepViewModel.class);
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    @SuppressLint("SetTextI18n")
    public void fillDateData(long date){
        Sleep sleep = dataSleepViewModel.findSleep(date);
        if(sleep == null){
            sleepTime.setText("Sleep Time");
            wakeTime.setText("Wake Time");
            durationView.setText("-");
        }
        else{
            sleepTime.setText(sleep.getSleepTime().toString());
            wakeTime.setText(sleep.getWakeTime().toString());
            //durationView.setText(sleep.getSleepDuration().toString());
        }
        buttonSave.setEnabled(false);
    }

    public void initialiseAll(){
        findAllViewByID();
        initialiseButtons();
    }

    public void findAllViewByID(){
        durationView = findViewById(R.id.durationView);
        buttonDate = findViewById(R.id.buttonDate);
        sleepTime = findViewById(R.id.sleepTime);
        wakeTime = findViewById(R.id.wakeTime);
        buttonSave = findViewById(R.id.buttonSave);
        buttonReturn = findViewById(R.id.buttonReturn);
    }

    public void initialiseButtons(){
        initialiseDateButton();
        initialiseSleepButton();
        initialiseWakeButton();
        initialiseBottomButtons();
    }

    @SuppressLint("DefaultLocale")
    public void initialiseDateButton(){
        Calendar currentDate = Calendar.getInstance();
        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);
        fillDateData(LocalDate.of(year, month, day).atStartOfDay(ZoneId.of("Asia/Singapore")).toInstant().toEpochMilli());
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));

        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
            fillDateData(LocalDate.of(year, month, day).atStartOfDay(ZoneId.of("Asia/Singapore")).toInstant().toEpochMilli());
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        }, year, month, day).show());
    }

    @SuppressLint("DefaultLocale")
    public void initialiseSleepButton(){
        sleepTime.setOnClickListener(view -> new TimePickerDialog(DataSleep.this, (timePicker, i, i1) -> {
            sleepHour = i;
            sleepMinute = i1;
            sleepTime.setText(String.format("%02d:%02d", sleepHour, sleepMinute));
            buttonSave.setEnabled(calculateSleepDuration());
        }, sleepHour, sleepMinute, false).show());
    }

    @SuppressLint("DefaultLocale")
    public void initialiseWakeButton(){
        wakeTime.setOnClickListener(view -> new TimePickerDialog(DataSleep.this, (timePicker, i, i1) -> {
            wakeHour = i;
            wakeMinute = i1;
            wakeTime.setText(String.format("%02d:%02d", wakeHour, wakeMinute));
            buttonSave.setEnabled(calculateSleepDuration());
        }, wakeHour, wakeMinute, false).show());
    }

    @SuppressLint("DefaultLocale")
    public boolean calculateSleepDuration(){
        int duration = (wakeHour - sleepHour) * 60 + (60 - sleepMinute + wakeMinute);
        duration += ((sleepHour >= 12 && wakeHour < 12) ? 23 : (sleepHour < 12 && wakeHour >= 12) ? 11 : (sleepHour * 60 + sleepMinute < wakeHour * 60 + wakeMinute) ? -1 : -24) * 60;
        boolean validDuration = duration > 0;
        durationView.setText(validDuration ? String.format("%02d:%02d", duration / 60, duration % 60) : "Invalid Time Range");
        return validDuration;
    }

    public void initialiseBottomButtons(){
        //buttonSave.setOnClickListener();
        buttonReturn.setOnClickListener(v -> finish());
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}