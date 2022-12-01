package com.example.myapp.fragments.sport.sportCalendar;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.subActivities.sport.SportDataActivity;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;

public class SportCalendarViewModel extends AndroidViewModel {

    private SportRepository sportRepository;
    private int userID;

    public SportCalendarViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public Sport findSport(long date){
        return sportRepository.findSport(userID, getDate(date));
    }

    public Intent sportData(long date){
        Intent intent = new Intent(getApplication(), SportDataActivity.class);
        intent.putExtra("date", getDate(date));
        return intent;
    }

    public long getDate(long date){
        LocalDate localDate = Instant.ofEpochMilli(date).atZone(ZoneId.systemDefault()).toLocalDate();
        return localDate.atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
    }
}
