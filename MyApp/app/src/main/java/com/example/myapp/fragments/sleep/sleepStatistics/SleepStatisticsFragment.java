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

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SleepStatisticsFragment#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SleepStatisticsFragment extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SleepStatisticsFragment() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepStatistics.
     */
    // TODO: Rename and change types and number of parameters
    public static SleepStatisticsFragment newInstance(String param1, String param2) {
        SleepStatisticsFragment fragment = new SleepStatisticsFragment();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SleepStatisticsViewModel sleepStatisticsViewModel;
    TextView sleepTotal, dayTotal, sleepLongest, sleepShortest, sleepAverage, sleepEarliest, sleepLatest, wakeEarliest, wakeLatest;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        initialiseViews();
        sleepStatisticsViewModel.getSleepList().observe(getViewLifecycleOwner(), sleepList -> updateResults(sleepStatisticsViewModel.processResults(sleepList)));
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

    public void updateResults(int[] results){
        if(results[7] == 0){
            sleepTotal.setText(String.valueOf(0));
            dayTotal.setText(String.valueOf(0));
            sleepLongest.setText(String.valueOf(0));
            sleepShortest.setText(String.valueOf(0));
            sleepAverage.setText(String.valueOf(0));
            sleepEarliest.setText(String.valueOf(0));
            sleepLatest.setText(String.valueOf(0));
            wakeEarliest.setText(String.valueOf(0));
            wakeLatest.setText(String.valueOf(0));
        }
        else{
            sleepTotal.setText(String.valueOf(results[0]));
            dayTotal.setText(String.valueOf(results[7]));
            sleepLongest.setText(String.valueOf(results[1]));
            sleepShortest.setText(String.valueOf(results[2]));
            sleepAverage.setText(String.valueOf(results[0] / (double) results[7]));
            sleepEarliest.setText(String.valueOf(results[3]));
            sleepLatest.setText(String.valueOf(results[4]));
            wakeEarliest.setText(String.valueOf(results[5]));
            wakeLatest.setText(String.valueOf(results[6]));
        }
    }
}