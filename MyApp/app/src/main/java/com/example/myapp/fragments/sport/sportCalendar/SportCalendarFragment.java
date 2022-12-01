package com.example.myapp.fragments.sport.sportCalendar;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CalendarView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.subActivities.sport.SportDataActivity;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.TimeZone;

public class SportCalendarFragment extends Fragment {

    SportCalendarViewModel sportCalendarViewModel;
    Button addButton, infoButton;
    CalendarView calendarView;
    Intent intent;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sportCalendarViewModel = new ViewModelProvider(this).get(SportCalendarViewModel.class);
        intent = new Intent(getContext(), SportDataActivity.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_calendar, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseCalendar();
        initialiseButtons();
        checkDateData(getCurrentDate());
    }

    public void initialiseCalendar(){
        calendarView = requireView().findViewById(R.id.calendarSport);
        calendarView.setOnDateChangeListener(onDateChangeListener);
    }

    public void initialiseButtons(){
        initialiseAddButton();
        initialiseInfoButton();
    }

    public void initialiseAddButton(){
        addButton = requireView().findViewById(R.id.addButton);
        addButton.setOnClickListener(view1 -> startActivity(sportCalendarViewModel.sportData(calendarView.getDate())));
    }

    public void initialiseInfoButton(){
        infoButton = requireView().findViewById(R.id.infoButton);
        infoButton.setOnClickListener(view1 -> startActivity(sportCalendarViewModel.sportData(calendarView.getDate())));
    }

    public void checkDateData(long date){
        boolean hasData = sportCalendarViewModel.findSport(date) != null;
        addButton.setEnabled(!hasData);
        infoButton.setEnabled(hasData);
    }

    public long getCurrentDate(){
        Calendar currentDate = Calendar.getInstance();
        return currentDate.toInstant().toEpochMilli();
    }

    CalendarView.OnDateChangeListener onDateChangeListener = (view, year, month, day) -> {
        checkDateData(LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli());
    };
}