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
import com.example.myapp.databasefiles.sleep.Sleep;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Objects;

public class SleepDataActivity extends AppCompatActivity{

    SleepDataViewModel sleepDataViewModel;

    Button buttonDate, sleepTime, wakeTime, buttonSave, buttonReturn;
    TextView durationView;

    int year, month, day;
    int sleepHour, sleepMinute;
    int wakeHour, wakeMinute;

    long date;
    int timeSleep;
    int timeWake;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sleep);
        sleepDataViewModel = new ViewModelProvider(this).get(SleepDataViewModel.class);
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
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

    public void initialiseDate(){
        long dateMillis = getIntent().getExtras().getLong("date");
        LocalDate date = Instant.ofEpochMilli(dateMillis).atZone(ZoneId.systemDefault()).toLocalDate();
        year = date.getYear();
        month = date.getMonthValue();
        day = date.getDayOfMonth();
        fillDateData(dateMillis);
    }

    @SuppressLint("DefaultLocale")
    public void initialiseDateButton(){
        initialiseDate();
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1 + 1;
            day = i2;
            fillDateData(LocalDate.of(year, month, day).atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli());
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        }, year, month - 1, day).show());
    }

    @SuppressLint("DefaultLocale")
    public void initialiseSleepButton(){
        sleepTime.setOnClickListener(view -> new TimePickerDialog(SleepDataActivity.this, (timePicker, i, i1) -> {
            sleepHour = i;
            sleepMinute = i1;
            sleepTime.setText(String.format("%02d:%02d", sleepHour, sleepMinute));
            buttonSave.setEnabled(calculateSleepDuration());
        }, sleepHour, sleepMinute, false).show());
    }

    @SuppressLint("DefaultLocale")
    public void initialiseWakeButton(){
        wakeTime.setOnClickListener(view -> new TimePickerDialog(SleepDataActivity.this, (timePicker, i, i1) -> {
            wakeHour = i;
            wakeMinute = i1;
            wakeTime.setText(String.format("%02d:%02d", wakeHour, wakeMinute));
            buttonSave.setEnabled(calculateSleepDuration());
        }, wakeHour, wakeMinute, false).show());
    }

    public void initialiseBottomButtons(){
        buttonSave.setOnClickListener(v -> {
            if(sleepDataViewModel.getSleep() == null)
                sleepDataViewModel.insert(date, timeSleep, timeWake);
            else
                sleepDataViewModel.update(timeSleep, timeWake);
            finish();
        });
        buttonReturn.setOnClickListener(v -> finish());
    }

    @SuppressLint({"SetTextI18n", "DefaultLocale"})
    public void fillDateData(long date){
        this.date = date;
        initialiseTime(sleepDataViewModel.loadSleepData(date));
        sleepTime.setText(String.format("%02d:%02d", sleepHour, sleepMinute));
        wakeTime.setText(String.format("%02d:%02d", wakeHour, wakeMinute));
        calculateSleepDuration();
        buttonSave.setEnabled(false);
    }

    public void initialiseTime(Sleep sleep){
        int sleepTime = sleep == null ? 0 : sleep.getSleepTime();
        sleepHour = sleepTime / 60;
        sleepMinute = sleepTime % 60;
        int wakeTime = sleep == null ? 0 : sleep.getWakeTime();
        wakeHour = wakeTime / 60;
        wakeMinute = wakeTime % 60;
    }

    @SuppressLint("DefaultLocale")
    public boolean calculateSleepDuration(){
        int duration = (wakeHour - sleepHour - 1) * 60 + (60 - sleepMinute + wakeMinute);
        duration += (duration >= 0) ? 0 : 1440;
        durationView.setText(duration > 0 ? String.format("%02d:%02d", duration / 60, duration % 60) : "-");
        timeSleep = sleepHour * 60 + sleepMinute;
        timeWake = wakeHour * 60 + wakeMinute;
        return duration > 0;
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