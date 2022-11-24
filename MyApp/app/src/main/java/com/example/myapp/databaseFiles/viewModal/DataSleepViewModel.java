package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.repository.SleepRepository;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

public class DataSleepViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;

    private int userID;
    private int sleepID;
    private LocalDate date;

    public DataSleepViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void insert(LocalTime sleepTime, LocalTime wakeTime){
        sleepRepository.insert(new Sleep(date, sleepTime, wakeTime, userID));
    }

    public void update(LocalTime sleepTime, LocalTime wakeTime){
        sleepRepository.update(new Sleep(sleepID, date, sleepTime, wakeTime, userID));
    }

    public void delete(LocalTime sleepTime, LocalTime wakeTime){
        sleepRepository.delete(new Sleep(sleepID, date, sleepTime, wakeTime, userID));
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Sleep findSleep(LocalDate date){
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
