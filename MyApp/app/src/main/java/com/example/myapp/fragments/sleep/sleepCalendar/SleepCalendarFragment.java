package com.example.myapp.fragments.sleep.sleepCalendar;

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
import java.util.Calendar;
import java.util.TimeZone;

public class SleepCalendarFragment extends Fragment {

    SleepCalendarViewModel sleepCalendarViewModel;
    Button addButton, infoButton;
    CalendarView calendarView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sleepCalendarViewModel = new ViewModelProvider(this).get(SleepCalendarViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep_calendar, container, false);
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
        calendarView = requireView().findViewById(R.id.calendarSleep);
        calendarView.setOnDateChangeListener(onDateChangeListener);
    }

    public void initialiseButtons(){
        initialiseAddButton();
        initialiseInfoButton();
    }

    public void initialiseAddButton(){
        addButton = requireView().findViewById(R.id.addButton);
        addButton.setOnClickListener(view1 -> startActivity(sleepCalendarViewModel.sleepData(calendarView.getDate())));
    }

    public void initialiseInfoButton(){
        infoButton = requireView().findViewById(R.id.infoButton);
        infoButton.setOnClickListener(view1 -> startActivity(sleepCalendarViewModel.sleepData(calendarView.getDate())));
    }

    public void checkDateData(long date){
        boolean hasData = sleepCalendarViewModel.findSleep(date) != null;
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