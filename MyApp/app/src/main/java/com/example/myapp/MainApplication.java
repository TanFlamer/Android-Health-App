package com.example.myapp;

import android.app.Application;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.example.myapp.databaseFiles.repository.UserRepository;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class MainApplication extends Application {

    private MutableLiveData<List<Pair<String, LocalTime>>> saveLogs;
    private int userID;

    @Override
    public void onCreate() {
        super.onCreate();
        userID = 0;
        saveLogs = new MutableLiveData<>();
        saveLogs.setValue(new ArrayList<>());
    }

    public void updateSaveLogs(Pair<String, LocalTime> newSaveLog){
        List<Pair<String, LocalTime>> oldSaveLogs = saveLogs.getValue();
        oldSaveLogs.add(newSaveLog);
        saveLogs.setValue(oldSaveLogs);
    }

    public MutableLiveData<List<Pair<String, LocalTime>>> getSaveLogs() {
        return saveLogs;
    }

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }
}
