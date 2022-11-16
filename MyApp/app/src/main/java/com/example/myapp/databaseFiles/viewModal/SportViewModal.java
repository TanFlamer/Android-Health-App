package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.repository.SportRepository;

import java.util.List;

public class SportViewModal extends AndroidViewModel {

    private SportRepository sportRepository;
    private List<Sport> allSport;

    public SportViewModal(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        allSport = sportRepository.getAllSport();
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

    public Sport findSport(int sportID){
        return sportRepository.findSport(sportID);
    }

    public List<Sport> getAllSport() {
        return allSport;
    }
}
