package com.example.myapp.fragmentsSport;

import android.graphics.Color;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.example.myapp.R;
import com.github.mikephil.charting.charts.BarChart;
import com.github.mikephil.charting.data.BarData;
import com.github.mikephil.charting.data.BarDataSet;
import com.github.mikephil.charting.data.BarEntry;
import com.github.mikephil.charting.utils.ColorTemplate;

import java.util.ArrayList;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportChart#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportChart extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportChart() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportChart.
     */
    // TODO: Rename and change types and number of parameters
    public static SportChart newInstance(String param1, String param2) {
        SportChart fragment = new SportChart();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_chart, container, false);
    }

    ArrayList<BarEntry> sportData;

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        BarChart barChart = requireView().findViewById(R.id.sportBarChart);
        getData();

        BarDataSet barDataSet = new BarDataSet(sportData, "Sleep Bar Chart");
        BarData barData = new BarData(barDataSet);
        barChart.setData(barData);
        barChart.setVisibleXRangeMaximum(10);

        barDataSet.setColors(ColorTemplate.COLORFUL_COLORS);
        barDataSet.setValueTextColor(Color.BLACK);
        barDataSet.setValueTextSize(16f);
        barChart.getDescription().setEnabled(true);
    }

    private void getData(){
        sportData = new ArrayList<>();
        sportData.add(new BarEntry(1f, 10));
        sportData.add(new BarEntry(2f, 20));
        sportData.add(new BarEntry(3f, 30));
        sportData.add(new BarEntry(4f, 40));
        sportData.add(new BarEntry(5f, 50));

        sportData.add(new BarEntry(6f, 10));
        sportData.add(new BarEntry(7f, 20));
        sportData.add(new BarEntry(8f, 30));
        sportData.add(new BarEntry(9f, 40));
        sportData.add(new BarEntry(10f, 50));

        sportData.add(new BarEntry(11f, 10));
        sportData.add(new BarEntry(12f, 20));
        sportData.add(new BarEntry(13f, 30));
        sportData.add(new BarEntry(14f, 40));
        sportData.add(new BarEntry(15f, 50));
    }
}