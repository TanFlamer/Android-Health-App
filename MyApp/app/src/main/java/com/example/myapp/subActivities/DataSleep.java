package com.example.myapp.subActivities;

import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Button;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;

import com.example.myapp.R;

import java.util.Calendar;
import java.util.Objects;

public class DataSleep extends AppCompatActivity{

    Button buttonDate, sleepTime, wakeTime, buttonEditSave, buttonReturn;
    TextView durationView;

    int year, month, day;
    int sleepHour, sleepMinute;
    int wakeHour, wakeMinute;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sleep);
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseButtons();
    }

    public void initialiseButtons(){
        initialiseDateButton();
        initialiseSleepButton();
        initialiseWakeButton();
        initialiseBottomButtons();
    }

    public void initialiseTextView(){
        durationView = findViewById(R.id.durationView);
    }

    public void initialiseDateButton(){
        Calendar currentDate = Calendar.getInstance();
        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);

        buttonDate = findViewById(R.id.buttonDate);
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
        }, year, month, day).show());
    }

    public void initialiseSleepButton(){
        sleepTime = findViewById(R.id.sleepTime);
        sleepTime.setOnClickListener(view -> new TimePickerDialog(DataSleep.this, (timePicker, i, i1) -> {
            sleepHour = i;
            sleepMinute = i1;
        }, sleepHour, sleepMinute, false).show());
    }

    public void initialiseWakeButton(){
        wakeTime = findViewById(R.id.wakeTime);
        wakeTime.setOnClickListener(view -> new TimePickerDialog(DataSleep.this, (timePicker, i, i1) -> {
            wakeHour = i;
            wakeMinute = i1;
        }, wakeHour, wakeMinute, false).show());
    }

    public void initialiseBottomButtons(){
        buttonEditSave = findViewById(R.id.buttonEditSave);
        buttonReturn = findViewById(R.id.buttonReturn);
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