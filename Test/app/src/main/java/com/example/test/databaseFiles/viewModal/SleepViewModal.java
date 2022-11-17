package com.example.test.databaseFiles.viewModal;

import android.app.Application;
import android.content.SharedPreferences;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.test.databaseFiles.entity.Sleep;
import com.example.test.databaseFiles.repository.SleepRepository;

import java.time.LocalDate;
import java.util.List;

public class SleepViewModal extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private LiveData<List<Sleep>> allSleep;

    public SleepViewModal(@NonNull Application application, int userID) {
        super(application);
        sleepRepository = new SleepRepository(application);
        allSleep = sleepRepository.getAllSleep(userID);
    }

    public void insert(Sleep sleep) {
        sleepRepository.insert(sleep);
    }

    public void update(Sleep sleep) {
        sleepRepository.update(sleep);
    }

    public void delete(Sleep sleep) {
        sleepRepository.delete(sleep);
    }

    public List<Sleep> findSleep(int userID, LocalDate date){
        return sleepRepository.findSleep(userID, date);
    }

    public LiveData<List<Sleep>> getAllSleep() {
        return allSleep;
    }
}
