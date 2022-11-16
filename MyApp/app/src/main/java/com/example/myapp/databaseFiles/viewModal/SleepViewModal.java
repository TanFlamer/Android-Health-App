package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.databaseFiles.entity.Sleep;
import com.example.myapp.databaseFiles.repository.SleepRepository;

import java.util.List;

public class SleepViewModal extends AndroidViewModel {

    private SleepRepository sleepRepository;
    private List<Sleep> allSleep;

    public SleepViewModal(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        allSleep = sleepRepository.getAllSleep();
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

    public Sleep findSleep(int sleepID){
        return sleepRepository.findSleep(sleepID);
    }

    public List<Sleep> getAllSleep() {
        return allSleep;
    }
}
