package com.example.myapp.subActivities.sleep;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sleep.Sleep;
import com.example.myapp.databaseFiles.sleep.SleepRepository;

import java.util.List;

public class DataSleepViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;

    private int userID;
    private int sleepID;
    private Long date;

    public DataSleepViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void insert(Integer sleepTime, Integer wakeTime){
        sleepRepository.insert(new Sleep(date, sleepTime, wakeTime, userID));
    }

    public void update(Integer sleepTime, Integer wakeTime){
        sleepRepository.update(new Sleep(sleepID, date, sleepTime, wakeTime, userID));
    }

    public void delete(Integer sleepTime, Integer wakeTime){
        sleepRepository.delete(new Sleep(sleepID, date, sleepTime, wakeTime, userID));
    }

    public void setDate(Long date) {
        this.date = date;
    }

    public Sleep findSleep(Long date){
        List<Sleep> sleepList = sleepRepository.findSleep(userID, date);
        if(sleepList.size() == 0){
            this.date = null;
            return null;
        }
        else {
            this.date = date;
            return sleepList.get(0);
        }
    }
}
