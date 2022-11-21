package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.repository.SleepRepository;

import java.time.LocalDate;
import java.util.List;

public class DataSleepViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private LocalDate date;
    private int userID;

    public DataSleepViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public List<Sleep> findSleep(LocalDate date){
        return sleepRepository.findSleep(userID, date);
    }
}
