package com.example.myapp.fragments.sleep.sleepChart;

import android.annotation.SuppressLint;
import android.graphics.Color;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;

import com.example.myapp.R;
import com.github.mikephil.charting.charts.BarChart;
import com.github.mikephil.charting.components.XAxis;
import com.github.mikephil.charting.components.YAxis;
import com.github.mikephil.charting.data.BarData;
import com.github.mikephil.charting.data.BarDataSet;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.formatter.DefaultValueFormatter;
import com.github.mikephil.charting.formatter.IndexAxisValueFormatter;
import com.github.mikephil.charting.formatter.ValueFormatter;
import com.github.mikephil.charting.utils.ColorTemplate;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SleepChart#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SleepChart extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SleepChart() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepChart.
     */
    // TODO: Rename and change types and number of parameters
    public static SleepChart newInstance(String param1, String param2) {
        SleepChart fragment = new SleepChart();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SleepChartViewModel sleepChartViewModel;
    List<BarEntry> sleepData;
    BarChart barChart;
    BarDataSet barDataSet;
    BarData barData;
    Spinner dataSpinner;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
        sleepChartViewModel = new ViewModelProvider(this).get(SleepChartViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep_chart, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseSpinners();
        barChart = requireView().findViewById(R.id.sleepBarChart);
        sleepData = new ArrayList<>();
        sleepData.add(new BarEntry(0,0));

        barDataSet = new BarDataSet(sleepData, "Sleep Bar Chart");
        barDataSet.setColors(ColorTemplate.COLORFUL_COLORS);
        barDataSet.setValueTextColor(Color.BLACK);
        barDataSet.setValueTextSize(16f);
        barDataSet.setValueFormatter(new DefaultValueFormatter(2));

        barData = new BarData(barDataSet);
        barChart.setData(barData);
        barChart.getXAxis().setPosition(XAxis.XAxisPosition.BOTTOM);
        barChart.getXAxis().setGranularity(1);
        barChart.getDescription().setEnabled(false);

        sleepChartViewModel.getSleepList().observe(getViewLifecycleOwner(), sleepList -> refreshBarChart(sleepChartViewModel.processData(sleepList, dataSpinner.getSelectedItem().toString())));
    }

    public void refreshBarChart(Pair<List<String>, List<BarEntry>> pair){
        resetXAxisMin(dataSpinner.getSelectedItem().toString().equals("Sleep Duration"));
        sleepData.clear();
        sleepData.addAll(pair.second);
        barDataSet.notifyDataSetChanged();
        barDataSet.setValueFormatter(dataSpinner.getSelectedItem().toString().equals("Sleep Duration") ? new DefaultValueFormatter(2) : yValueFormatter);
        barData.notifyDataChanged();
        barChart.notifyDataSetChanged();
        barChart.invalidate();
        barChart.setVisibleXRangeMaximum(5);
        barChart.moveViewToX(pair.first.size() - 1);
        barChart.getXAxis().setValueFormatter(new ValueFormatter() {
            @Override
            public String getFormattedValue(float value) {
                return pair.first.size() == 0 ? "" : pair.first.get((int) value);
            }
        });
    }

    public void resetXAxisMin(boolean isDuration){
        if(isDuration){
            barChart.getAxisLeft().setAxisMinimum(0);
            barChart.getAxisRight().setAxisMinimum(0);
        }
        else{
            barChart.getAxisLeft().resetAxisMinimum();
            barChart.getAxisRight().resetAxisMinimum();
        }
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Sleep Duration", "Sleep Time", "Wake Time"};
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            refreshBarChart(sleepChartViewModel.changeData(dataSpinner.getSelectedItem().toString()));
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    ValueFormatter yValueFormatter = new ValueFormatter() {
        @SuppressLint("DefaultLocale")
        @Override
        public String getFormattedValue(float value) {
            int time = (int) (value * 60);
            time += dataSpinner.getSelectedItem().toString().equals("Wake Time") ? 720 : (time < 0 ? 1440 : 0);
            return String.format("%02d:%02d", time / 60, time % 60);
        }
    };
}