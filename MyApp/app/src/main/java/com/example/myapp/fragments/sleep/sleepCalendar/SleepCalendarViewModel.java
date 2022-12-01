package com.example.myapp.fragments.sleep.sleepCalendar;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

public class SleepCalendarViewModel extends AndroidViewModel {

    private final SleepRepository sleepRepository;
    private final int userID;

    public SleepCalendarViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public Sleep findSleep(long date){
        return sleepRepository.findSleep(userID, getDate(date));
    }

    public Intent sleepData(long date){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        intent.putExtra("date", getDate(date));
        return intent;
    }

    public long getDate(long date){
        LocalDate localDate = Instant.ofEpochMilli(date).atZone(ZoneId.systemDefault()).toLocalDate();
        return localDate.atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
    }
}
