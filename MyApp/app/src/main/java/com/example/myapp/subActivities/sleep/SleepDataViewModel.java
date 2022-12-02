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

    public SleepDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        sleepRepository = mainApplication.getSleepRepository();
        userID = mainApplication.getUserID();
    }

    public Sleep loadSleepData(long date){
        sleep = sleepRepository.findSleep(userID, date);
        return sleep;
    }

    public void insert(long date, int sleepTime, int wakeTime){
        LocalDate localDate = Instant.ofEpochMilli(date).atZone(ZoneId.systemDefault()).toLocalDate();
        updateSaveLogs("Sleep data for " + localDate + " added");
        sleepRepository.insert(new Sleep(date, sleepTime, wakeTime, userID));
    }

    public void update(int sleepTime, int wakeTime){
        updateSaveLogs("Sleep data for " + getDate() + " updated");
        sleepRepository.update(new Sleep(sleep.getSleepID(), sleep.getDate(), sleepTime, wakeTime, userID));
    }

    public void delete(){
        updateSaveLogs("Sleep data for " + getDate() + " deleted");
        sleepRepository.delete(sleep);
    }

    public LocalDate getDate(){
        return Instant.ofEpochMilli(sleep.getDate()).atZone(ZoneId.systemDefault()).toLocalDate();
    }

    public Sleep getSleep() {
        return sleep;
    }

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
