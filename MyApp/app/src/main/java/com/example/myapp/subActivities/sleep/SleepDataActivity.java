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
        //get view model
        sleepDataViewModel = new ViewModelProvider(this).get(SleepDataViewModel.class);
        //show back button on top
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components by ID
        findAllViewByID();
        //initialise all buttons
        initialiseButtons();
    }

    //link all components by ID
    public void findAllViewByID(){
        durationView = findViewById(R.id.durationView);
        buttonDate = findViewById(R.id.buttonDate);
        sleepTime = findViewById(R.id.sleepTime);
        wakeTime = findViewById(R.id.wakeTime);
        buttonSave = findViewById(R.id.buttonSave);
        buttonReturn = findViewById(R.id.buttonReturn);
    }

    //initialise all buttons
    public void initialiseButtons(){
        //initialise data button
        initialiseDateButton();
        //initialise sleep time button
        initialiseSleepButton();
        //initialise wake time button
        initialiseWakeButton();
        //initialise save and return button
        initialiseBottomButtons();
    }

    //initialise calendar view with initial date
    public void initialiseDate(){
        //get date from intent
        long dateMillis = getIntent().getExtras().getLong("date");
        LocalDate date = Instant.ofEpochMilli(dateMillis).atZone(ZoneId.systemDefault()).toLocalDate();
        //set year, month and day
        year = date.getYear();
        month = date.getMonthValue();
        day = date.getDayOfMonth();
        //update activity with sleep and wake time from given date
        fillDateData(dateMillis);
    }

    //initialise data button
    @SuppressLint("DefaultLocale")
    public void initialiseDateButton(){
        //initialise calendar view with initial date
        initialiseDate();
        //show date on button
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        //set on click listener
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            //set year, month and day
            year = i;
            month = i1 + 1;
            day = i2;
            //update activity with sleep and wake time from given date
            fillDateData(LocalDate.of(year, month, day).atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli());
            //show new date on button
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        }, year, month - 1, day).show());
    }

    //initialise sleep time button
    @SuppressLint("DefaultLocale")
    public void initialiseSleepButton(){
        //set on click listener
        sleepTime.setOnClickListener(view -> new TimePickerDialog(SleepDataActivity.this, (timePicker, i, i1) -> {
            //set sleep hour and minute
            sleepHour = i;
            sleepMinute = i1;
            //show new sleep time on button
            sleepTime.setText(String.format("%02d:%02d", sleepHour, sleepMinute));
            //enable save button if valid sleep duration
            buttonSave.setEnabled(calculateSleepDuration());
        }, sleepHour, sleepMinute, false).show());
    }

    //initialise wake time button
    @SuppressLint("DefaultLocale")
    public void initialiseWakeButton(){
        //set on click listener
        wakeTime.setOnClickListener(view -> new TimePickerDialog(SleepDataActivity.this, (timePicker, i, i1) -> {
            //set wake hour and minute
            wakeHour = i;
            wakeMinute = i1;
            //show new wake time on button
            wakeTime.setText(String.format("%02d:%02d", wakeHour, wakeMinute));
            //enable save button if valid sleep duration
            buttonSave.setEnabled(calculateSleepDuration());
        }, wakeHour, wakeMinute, false).show());
    }

    //initialise save and return button
    public void initialiseBottomButtons(){
        //set save button on click listener
        buttonSave.setOnClickListener(v -> {
            if(sleepDataViewModel.getSleep() == null) //insert new sleep data if no date data
                sleepDataViewModel.insert(date, timeSleep, timeWake);
            else //update existing sleep data if date data exists
                sleepDataViewModel.update(timeSleep, timeWake);
            finish(); //return to last activity
        });
        //set return button on click listener to return to last activity
        buttonReturn.setOnClickListener(v -> finish());
    }

    //update activity with sleep and wake time from given date
    @SuppressLint({"SetTextI18n", "DefaultLocale"})
    public void fillDateData(long date){
        //set current date
        this.date = date;
        //get sleep and wake time from view model
        initialiseTime(sleepDataViewModel.loadSleepData(date));
        //show new sleep time on button
        sleepTime.setText(String.format("%02d:%02d", sleepHour, sleepMinute));
        //show new wake time on button
        wakeTime.setText(String.format("%02d:%02d", wakeHour, wakeMinute));
        //calculate sleep duration
        calculateSleepDuration();
        //disable save button
        buttonSave.setEnabled(false);
    }

    //set sleep and wake hour and minute
    public void initialiseTime(Sleep sleep){
        int sleepTime = sleep == null ? 0 : sleep.getSleepTime();
        sleepHour = sleepTime / 60;
        sleepMinute = sleepTime % 60;
        int wakeTime = sleep == null ? 0 : sleep.getWakeTime();
        wakeHour = wakeTime / 60;
        wakeMinute = wakeTime % 60;
    }

    //calculate sleep duration
    @SuppressLint("DefaultLocale")
    public boolean calculateSleepDuration(){
        int duration = (wakeHour - sleepHour - 1) * 60 + (60 - sleepMinute + wakeMinute);
        duration += (duration >= 0) ? 0 : 1440;
        //show new duration on text view
        durationView.setText(duration > 0 ? String.format("%02d:%02d", duration / 60, duration % 60) : "-");
        timeSleep = sleepHour * 60 + sleepMinute;
        timeWake = wakeHour * 60 + wakeMinute;
        //duration is valid if sleep and wake time not equal
        return duration > 0;
    }

    @Override //set back button on top to return to last activity
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    //initialise options menu
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}