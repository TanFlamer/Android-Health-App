package com.example.myapp.subActivities.sleep;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

public class SleepDataViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SleepRepository sleepRepository;
    private final int userID;
    private Sleep sleep;

    //constructor for view model
    public SleepDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        sleepRepository = mainApplication.getSleepRepository();
        userID = mainApplication.getUserID();
    }

    //load sleep data from database
    public Sleep loadSleepData(long date){
        sleep = sleepRepository.findSleep(userID, date);
        return sleep;
    }

    //insert new sleep data to database
    public void insert(long date, int sleepTime, int wakeTime){
        LocalDate localDate = Instant.ofEpochMilli(date).atZone(ZoneId.systemDefault()).toLocalDate();
        updateSaveLogs("Sleep data for " + localDate + " added");
        sleepRepository.insert(new Sleep(date, sleepTime, wakeTime, userID));
    }

    //update existing sleep data in database
    public void update(int sleepTime, int wakeTime){
        updateSaveLogs("Sleep data for " + getDate() + " updated");
        sleep.setSleepTime(sleepTime);
        sleep.setWakeTime(wakeTime);
        sleepRepository.update(sleep);
    }

    //convert long to date
    public LocalDate getDate(){
        return Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
    }

    //return sleep data
    public Sleep getSleep() {
        return sleep;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
