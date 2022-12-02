package com.example.myapp.fragments.sport.sportChart;

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
import com.github.mikephil.charting.data.BarData;
import com.github.mikephil.charting.data.BarDataSet;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.formatter.DefaultValueFormatter;
import com.github.mikephil.charting.formatter.ValueFormatter;
import com.github.mikephil.charting.utils.ColorTemplate;

import java.util.ArrayList;
import java.util.List;

public class SportChartFragment extends Fragment {

    SportChartViewModel sportChartViewModel;
    List<BarEntry> sportData;
    Spinner dataSpinner;
    BarChart barChart;
    BarDataSet barDataSet;
    BarData barData;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sportChartViewModel = new ViewModelProvider(this).get(SportChartViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_chart, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseBarDataSet();
        initialiseBarChart();
        initialiseSpinners();
        initialiseLiveData();
    }

    public void initialiseBarDataSet(){
        sportData = new ArrayList<>();
        sportData.add(new BarEntry(0,0));
        barDataSet = new BarDataSet(sportData, "Sport Bar Chart");
        barDataSet.setColors(ColorTemplate.COLORFUL_COLORS);
        barDataSet.setValueTextColor(Color.BLACK);
        barDataSet.setValueTextSize(16f);
        barDataSet.setValueFormatter(new DefaultValueFormatter(2));
    }

    public void initialiseBarChart(){
        barChart = requireView().findViewById(R.id.sportBarChart);
        barData = new BarData(barDataSet);
        barChart.setData(barData);
        barChart.getAxisLeft().setAxisMinimum(0);
        barChart.getAxisRight().setAxisMinimum(0);
        barChart.getXAxis().setPosition(XAxis.XAxisPosition.BOTTOM);
        barChart.getXAxis().setGranularity(1);
        barChart.getDescription().setEnabled(false);
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Sport Duration", "Sport Calorie"};
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    public void initialiseLiveData(){
        sportChartViewModel.getSportDateMerger().observe(getViewLifecycleOwner(), sportDataMerger -> {
            String data = dataSpinner.getSelectedItem().toString();
            refreshBarChart(sportChartViewModel.processData(sportDataMerger, data));
        });
    }

    public void refreshBarChart(Pair<List<String>, List<BarEntry>> pair){
        refreshBarDataSet(pair.second);
        refreshBarChart(pair.first);
    }

    public void refreshBarDataSet(List<BarEntry> barEntryList){
        sportData.clear();
        sportData.addAll(barEntryList);
        barDataSet.notifyDataSetChanged();
    }

    public void refreshBarChart(List<String> xAxisLabels){
        barData.notifyDataChanged();
        barChart.notifyDataSetChanged();
        barChart.invalidate();
        barChart.setVisibleXRangeMaximum(5);
        barChart.moveViewToX(xAxisLabels.size() - 1);
        barChart.getXAxis().setValueFormatter(new ValueFormatter() {
            @Override
            public String getFormattedValue(float value) {
                return xAxisLabels.size() == 0 ? "" : xAxisLabels.get((int) value);
            }
        });
    }

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            String data = dataSpinner.getSelectedItem().toString();
            sportChartViewModel.updateSaveLogs("Displaying Sport Chart by " + data);
            refreshBarChart(sportChartViewModel.changeData(data));
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}