package com.example.myapp.fragments.sleep.sleepStatistics;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;

public class SleepStatisticsFragment extends Fragment {

    SleepStatisticsViewModel sleepStatisticsViewModel;
    TextView sleepTotal, dayTotal, sleepLongest, sleepShortest, sleepAverage, sleepEarliest, sleepLatest, wakeEarliest, wakeLatest;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        sleepStatisticsViewModel = new ViewModelProvider(this).get(SleepStatisticsViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep_statistics, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //get ID for sleep statistics text views
        initialiseViews();
        //observe live data for sleep data list
        initialiseLiveData();
    }

    //get ID for sleep statistics text views
    public void initialiseViews(){
        sleepTotal = requireView().findViewById(R.id.sleepTotal);
        dayTotal = requireView().findViewById(R.id.dayTotal);
        sleepLongest = requireView().findViewById(R.id.sleepLongest);
        sleepShortest = requireView().findViewById(R.id.sleepShortest);
        sleepAverage = requireView().findViewById(R.id.sleepAverage);
        sleepEarliest = requireView().findViewById(R.id.sleepEarliest);
        sleepLatest = requireView().findViewById(R.id.sleepLatest);
        wakeEarliest = requireView().findViewById(R.id.wakeEarliest);
        wakeLatest = requireView().findViewById(R.id.wakeLatest);
    }

    //observe live data for sleep data list
    public void initialiseLiveData(){
        sleepStatisticsViewModel.getSleepLiveData().observe(getViewLifecycleOwner(), this::updateResults);
    }

    //update sleep statistics if sleep data list changes
    @SuppressLint("DefaultLocale")
    public void updateResults(String[] results){
        sleepTotal.setText(results[0]);
        dayTotal.setText(results[1]);
        sleepLongest.setText(results[2]);
        sleepShortest.setText(results[3]);
        sleepAverage.setText(results[4]);
        sleepEarliest.setText(results[5]);
        sleepLatest.setText(results[6]);
        wakeEarliest.setText(results[7]);
        wakeLatest.setText(results[8]);
    }
}