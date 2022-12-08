package com.example.myapp.fragments.sport.sportCalendar;

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

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.TimeZone;

public class SportCalendarFragment extends Fragment {

    SportCalendarViewModel sportCalendarViewModel;
    Button addButton, infoButton;
    CalendarView calendarView;
    int year, month, day;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        sportCalendarViewModel = new ViewModelProvider(this).get(SportCalendarViewModel.class);
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
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //initialise calendar view
        initialiseCalendar();
        //initialise add and info button
        initialiseButtons();
        //enable button if current date has sport data
        checkDateData(getCurrentDate());
    }

    //initialise calendar view
    public void initialiseCalendar(){
        //get calendar view by ID
        calendarView = requireView().findViewById(R.id.calendarSport);
        //add on date change listener
        calendarView.setOnDateChangeListener(onDateChangeListener);
    }

    //initialise add and info button
    public void initialiseButtons(){
        //initialise add button
        initialiseAddButton();
        //initialise info button
        initialiseInfoButton();
    }

    //initialise add button
    public void initialiseAddButton(){
        //get add button by ID
        addButton = requireView().findViewById(R.id.addButton);
        //go to edit sport data activity on click
        addButton.setOnClickListener(view1 -> {
            long date = LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
            startActivity(sportCalendarViewModel.sportData(date));
        });
    }

    //initialise info button
    public void initialiseInfoButton(){
        //get info button by ID
        infoButton = requireView().findViewById(R.id.infoButton);
        //go to edit sport data activity on click
        infoButton.setOnClickListener(view1 -> {
            long date = LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
            startActivity(sportCalendarViewModel.sportData(date));
        });
    }

    //enable button if selected date has sport data
    public void checkDateData(long date){
        //check if selected date has sport data
        boolean hasData = sportCalendarViewModel.findSport(date) != null;
        //enable add button if no sport data
        addButton.setEnabled(!hasData);
        //enable info button if has sport data
        infoButton.setEnabled(hasData);
    }

    //get current date
    public long getCurrentDate(){
        LocalDate localDate = LocalDate.now();
        year = localDate.getYear();
        month = localDate.getMonthValue();
        day = localDate.getDayOfMonth();
        return localDate.atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
    }

    //on date change listener
    CalendarView.OnDateChangeListener onDateChangeListener = (view, year, month, day) -> {
        //get selected year, month and day
        this.year = year;
        this.month = month + 1;
        this.day = day;
        long date = LocalDate.of(year, month + 1, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
        //enable button if selected date has sleep data
        checkDateData(date);
    };
}