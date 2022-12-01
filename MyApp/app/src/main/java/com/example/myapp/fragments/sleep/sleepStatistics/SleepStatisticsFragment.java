package com.example.myapp.fragments.sleep.sleepStatistics;

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
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseViews();
        initialiseLiveData();
    }

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

    public void initialiseLiveData(){
        sleepStatisticsViewModel.getSleepLiveData().observe(getViewLifecycleOwner(), this::updateResults);
    }

    public void updateResults(double[] results){
        sleepTotal.setText(String.valueOf(results[0]));
        dayTotal.setText(String.valueOf(results[1]));
        sleepLongest.setText(String.valueOf(results[2]));
        sleepShortest.setText(String.valueOf(results[3]));
        sleepAverage.setText(String.valueOf(results[4]));
        sleepEarliest.setText(String.valueOf(results[5]));
        sleepLatest.setText(String.valueOf(results[6]));
        wakeEarliest.setText(String.valueOf(results[7]));
        wakeLatest.setText(String.valueOf(results[8]));
    }
}