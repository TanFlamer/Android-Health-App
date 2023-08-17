package com.example.myapp.fragments.sport.sportCalendar;

import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.subActivities.sport.SportDataActivity;

public class SportCalendarViewModel extends AndroidViewModel {

    private final SportRepository sportRepository;
    private final int userID;

    //constructor for sport calendar view model
    public SportCalendarViewModel(@NonNull Application application) {
        super(application);
        sportRepository = new SportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    //check if selected date has sport data
    public Sport findSport(long date){
        return sportRepository.findSport(userID, date);
    }

    //send date to edit sport data activity
    public Intent sportData(long date){
        Intent intent = new Intent(getApplication(), SportDataActivity.class);
        intent.putExtra("date", date);
        return intent;
    }
}
