package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.repository.SportRepository;

import java.time.LocalDate;
import java.util.List;

public class SportViewModal extends AndroidViewModel {

    private SportRepository sportRepository;
    private LiveData<List<Sport>> allSport;

    public SportViewModal(@NonNull Application application, int userID) {
        super(application);
        sportRepository = new SportRepository(application);
        allSport = sportRepository.getAllSport(userID);
    }

    public void insert(Sport sport) {
        sportRepository.insert(sport);
    }

    public void update(Sport sport) {
        sportRepository.update(sport);
    }

    public void delete(Sport sport) {
        sportRepository.delete(sport);
    }

    public List<Sport> findSport(int userID, LocalDate date){
        return sportRepository.findSport(userID, date);
    }

    public LiveData<List<Sport>> getAllSport() {
        return allSport;
    }
}
