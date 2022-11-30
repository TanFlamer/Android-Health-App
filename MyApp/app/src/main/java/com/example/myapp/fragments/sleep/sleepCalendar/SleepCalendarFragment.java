package com.example.myapp.fragments.sleep.sleepCalendar;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CalendarView;

import com.example.myapp.R;
import com.example.myapp.subActivities.sleep.SleepDataActivity;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.TimeZone;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SleepCalendarFragment#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SleepCalendarFragment extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SleepCalendarFragment() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepCalendar.
     */
    // TODO: Rename and change types and number of parameters
    public static SleepCalendarFragment newInstance(String param1, String param2) {
        SleepCalendarFragment fragment = new SleepCalendarFragment();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SleepCalendarViewModel sleepCalendarViewModel;
    Button addButton, infoButton;
    CalendarView calendarView;
    Intent intent;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
        sleepCalendarViewModel = new ViewModelProvider(this).get(SleepCalendarViewModel.class);
        intent = new Intent(getContext(), SleepDataActivity.class);
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

    public long getCurrentDate(){
        Calendar currentDate = Calendar.getInstance();
        int year = currentDate.get(Calendar.YEAR);
        int month = currentDate.get(Calendar.MONTH);
        int day = currentDate.get(Calendar.DAY_OF_MONTH);
        return LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
    }

    public void initialiseAll(){
        initialiseCalendar();
        initialiseButton();
        checkDateData(getCurrentDate());
    }

    public void initialiseCalendar(){
        calendarView = requireView().findViewById(R.id.calendarSleep);
        calendarView.setOnDateChangeListener(onDateChangeListener);
    }

    public void initialiseButton(){
        addButton = requireView().findViewById(R.id.addButton);
        addButton.setOnClickListener(view1 -> startActivity(intent));
        infoButton = requireView().findViewById(R.id.infoButton);
        infoButton.setOnClickListener(view1 -> startActivity(intent));
    }

    CalendarView.OnDateChangeListener onDateChangeListener = (view, year, month, day) -> {
        intent.putExtra("year", year);
        intent.putExtra("month", month);
        intent.putExtra("day", day);
        checkDateData(LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli());
    };

    public void checkDateData(long date){
        boolean hasData = sleepCalendarViewModel.findSleep(date) != null;
        addButton.setEnabled(!hasData);
        infoButton.setEnabled(hasData);
    }
}