package com.example.myapp.fragments.sleep.sleepCalendar;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepRepository;

import java.util.List;

public class SleepCalendarViewModel extends AndroidViewModel {

    SleepRepository sleepRepository;
    private int userID;

    public SleepCalendarViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public List<Sleep> findSleep(Long date){
        return sleepRepository.findSleep(userID, date);
    }
}