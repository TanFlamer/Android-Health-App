package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.repository.SleepRepository;

import java.time.LocalDate;
import java.util.List;

public class SleepListViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private LiveData<List<Sleep>> sleepList;
    private int userID;

    public SleepListViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        userID = loadUserID();
        sleepList = sleepRepository.getAllSleep(userID);
    }

    public int loadUserID(){
        MainApplication appState = this.getApplication();
        return appState.getUserID();
    }

    public void insert(Sleep sleep){
        sleepRepository.insert(sleep);
    }

    public void update(Sleep sleep){
        sleepRepository.update(sleep);
    }

    public void delete(Sleep sleep){
        sleepRepository.delete(sleep);
    }

    public List<Sleep> findSleep(int userID, LocalDate date){
        return sleepRepository.findSleep(userID, date);
    }

    public LiveData<List<Sleep>> getSleepList(){
        return sleepList;
    }

    public int getUserID() {
        return userID;
    }
}
