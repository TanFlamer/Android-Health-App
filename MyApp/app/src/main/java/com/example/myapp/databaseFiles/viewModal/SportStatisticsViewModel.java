package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Sport;
import com.example.myapp.databaseFiles.repository.SportRepository;

import java.util.List;

public class SportStatisticsViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private LiveData<List<Sport>> sportList;
    private int userID;

    public SportStatisticsViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        sportList = sportRepository.getAllSport(userID);
    }

    public LiveData<List<Sport>> getSportList(){
        return sportList;
    }

    public int getUserID() {
        return userID;
    }
}
