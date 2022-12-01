package com.example.myapp.fragments.sport.sportCalendar;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.subActivities.sport.SportDataActivity;

public class SportCalendarViewModel extends AndroidViewModel {

    private final SportRepository sportRepository;
    private final int userID;

    public SportCalendarViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public Sport findSport(long date){
        return sportRepository.findSport(userID, date);
    }

    public Intent sportData(long date){
        Intent intent = new Intent(getApplication(), SportDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }
}
