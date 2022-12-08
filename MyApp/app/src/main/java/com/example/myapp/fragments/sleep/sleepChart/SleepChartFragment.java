package com.example.myapp.fragments.sleep.sleepChart;

import android.graphics.Color;
import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.github.mikephil.charting.charts.BarChart;
import com.github.mikephil.charting.components.XAxis;
import com.github.mikephil.charting.data.BarData;
import com.github.mikephil.charting.data.BarDataSet;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.formatter.DefaultValueFormatter;
import com.github.mikephil.charting.formatter.ValueFormatter;
import com.github.mikephil.charting.utils.ColorTemplate;

import java.util.ArrayList;
import java.util.List;

public class SleepChartFragment extends Fragment {

    SleepChartViewModel sleepChartViewModel;
    List<BarEntry> sleepData;
    BarChart barChart;
    BarDataSet barDataSet;
    BarData barData;
    Spinner dataSpinner;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
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
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //initialise bar data set
        initialiseBarDataSet();
        //initialise bar chart
        initialiseBarChart();
        //initialise data spinner
        initialiseSpinners();
        //observe sleep list live data
        initialiseLiveData();
    }

    //initialise bar data set
    public void initialiseBarDataSet(){
        //create sleep data list
        sleepData = new ArrayList<>();
        //add dummy data to initialise chart
        sleepData.add(new BarEntry(0,0));
        //create bar data set
        barDataSet = new BarDataSet(sleepData, "Sleep Bar Chart");
        //set bar colours
        barDataSet.setColors(ColorTemplate.COLORFUL_COLORS);
        //set text colour
        barDataSet.setValueTextColor(Color.BLACK);
        //set text size
        barDataSet.setValueTextSize(16f);
        //set initial value formatter
        barDataSet.setValueFormatter(new DefaultValueFormatter(2));
    }

    //initialise bar chart
    public void initialiseBarChart(){
        //get bar chart by ID
        barChart = requireView().findViewById(R.id.sleepBarChart);
        //create bar data from bar data set
        barData = new BarData(barDataSet);
        //set bar chart with bar data
        barChart.setData(barData);
        //move x-axis to bottom
        barChart.getXAxis().setPosition(XAxis.XAxisPosition.BOTTOM);
        //set x-axis granularity to 1
        barChart.getXAxis().setGranularity(1);
        //disable bar chart description
        barChart.getDescription().setEnabled(false);
    }

    //initialise data spinner
    public void initialiseSpinners(){
        //spinner data choices
        String[] data = new String[] {"Sleep Duration", "Sleep Time", "Wake Time"};
        //get spinner by ID
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        //set spinner with adapter
        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        //set on item selected listener to spinner
        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    //observe sleep list live data
    public void initialiseLiveData(){
        //update bar chart if sleep list changes
        sleepChartViewModel.getSleepList().observe(getViewLifecycleOwner(), sleepList -> {
            //get data type to view
            String data = dataSpinner.getSelectedItem().toString();
            //refresh bar chart
            refreshBarChart(sleepChartViewModel.processData(sleepList, data));
        });
    }

    //refresh bar chart
    public void refreshBarChart(Pair<List<String>, List<BarEntry>> pair){
        //get data type to view
        String data = dataSpinner.getSelectedItem().toString();
        //reset x-axis min value
        resetXAxisMin(data.equals("Sleep Duration"));
        //refresh bar data set
        refreshBarDataSet(pair.second);
        //refresh bar chart
        refreshBarChart(pair.first);
    }

    //refresh bar data set
    public void refreshBarDataSet(List<BarEntry> barEntryList){
        //get data type to view
        String data = dataSpinner.getSelectedItem().toString();
        //clear old sleep data
        sleepData.clear();
        //add new sleep data
        sleepData.addAll(barEntryList);
        //notify bar data set data changed
        barDataSet.notifyDataSetChanged();
        //set new value formatter
        barDataSet.setValueFormatter(sleepChartViewModel.getValueFormatter(data));
    }

    //refresh bar chart
    public void refreshBarChart(List<String> xAxisLabels){
        //notify bar data data changed
        barData.notifyDataChanged();
        //notify bar chart data changed
        barChart.notifyDataSetChanged();
        //invalidate old bar chart
        barChart.invalidate();
        //set visible bars to 5
        barChart.setVisibleXRangeMaximum(5);
        //move view to last bar
        barChart.moveViewToX(Math.max(0, xAxisLabels.size() - 1));
        //get x-axis value formatter
        barChart.getXAxis().setValueFormatter(new ValueFormatter() {
            @Override
            public String getFormattedValue(float value) {
                return xAxisLabels.size() == 0 ? "" : xAxisLabels.get((int) value);
            }
        });
    }

    //reset x-axis min value
    public void resetXAxisMin(boolean isDuration){
        if(isDuration){ //if sleep duration, min value is 0
            barChart.getAxisLeft().setAxisMinimum(0);
            barChart.getAxisRight().setAxisMinimum(0);
        }
        else{ //else reset x-axis min value
            barChart.getAxisLeft().resetAxisMinimum();
            barChart.getAxisRight().resetAxisMinimum();
        }
    }

    //on item selected listener for spinners
    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            //get bar data
            String data = dataSpinner.getSelectedItem().toString();
            //refresh bar chart
            refreshBarChart(sleepChartViewModel.changeData(data));
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}