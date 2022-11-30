package com.example.myapp.fragments.sport.sportCalendar;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;

public class SportCalendarViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private int userID;

    public SportCalendarViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public Sport findSport(long date){
        return sportRepository.findSport(userID, date);
    }
}
