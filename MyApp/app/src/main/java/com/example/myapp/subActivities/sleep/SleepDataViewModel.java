package com.example.myapp.subActivities.sleep;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sleep.Sleep;
import com.example.myapp.databasefiles.sleep.SleepRepository;

public class SleepDataViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private Sleep sleep;
    private int userID;

    public SleepDataViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public Sleep loadSleepData(long date){
        sleep = sleepRepository.findSleep(userID, date);
        return sleep;
    }

    public void insert(long date, int sleepTime, int wakeTime){
        sleepRepository.insert(new Sleep(date, sleepTime, wakeTime, userID));
    }

    public void update(int sleepTime, int wakeTime){
        sleepRepository.update(new Sleep(sleep.getSleepID(), sleep.getDate(), sleepTime, wakeTime, userID));
    }

    public void delete(){
        sleepRepository.delete(sleep);
    }

    public Sleep getSleep() {
        return sleep;
    }
}
