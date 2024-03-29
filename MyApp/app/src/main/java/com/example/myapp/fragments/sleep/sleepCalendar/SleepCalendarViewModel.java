package com.example.myapp.fragments.sleep.sleepCalendar;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepRepository;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

public class SleepCalendarViewModel extends AndroidViewModel {

    private final SleepRepository sleepRepository;
    private final int userID;

    //constructor for sleep calendar view model
    public SleepCalendarViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    //check if selected date has sleep data
    public Sleep findSleep(long date){
        return sleepRepository.findSleep(userID, date);
    }

    //send date to edit sleep data activity
    public Intent sleepData(long date){
        Intent intent = new Intent(getApplication(), SleepDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }
}
