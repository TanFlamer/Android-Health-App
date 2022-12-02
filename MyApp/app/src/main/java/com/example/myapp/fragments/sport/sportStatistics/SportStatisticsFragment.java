package com.example.myapp.fragments.sport.sportStatistics;

import android.os.Bundle;
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
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;

import java.util.HashMap;

public class SportStatisticsFragment extends Fragment {

    SportStatisticsViewModel sportStatisticsViewModel;
    Spinner dataSpinner, orderSpinner;
    RecyclerView recyclerView;
    SportStatisticsAdapter sportStatisticsAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sportStatisticsViewModel = new ViewModelProvider(this).get(SportStatisticsViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_statistics, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseRecyclerView();
        initialiseSpinners();
    }

    public void initialiseRecyclerView(){
        recyclerView = requireView().findViewById(R.id.sportRecyclerView);
        sportStatisticsAdapter = new SportStatisticsAdapter(requireContext(), new HashMap<>(), sportStatisticsViewModel);
        recyclerView.setAdapter(sportStatisticsAdapter);
        recyclerView.setHasFixedSize(true);
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        sportStatisticsViewModel.getSportDateMerger().observe(getViewLifecycleOwner(), typeHashMap -> {
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            sportStatisticsAdapter.updateSportList(typeHashMap, data, order);
        });
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Date Added", "Name", "Total Days", "Total Duration", "Total Calorie", "Average Duration", "Average Calorie", "Max Duration", "Max Calorie", "Min Duration", "Min Calorie"};
        String[] order = new String[] {"Ascending", "Descending"};

        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        orderSpinner = requireView().findViewById(R.id.orderSpinner);

        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        orderSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, order));

        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
        orderSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            sportStatisticsViewModel.updateSaveLogs("Sport Statistics sorted by " + data + " in " + order + " order");
            sportStatisticsAdapter.sortSportList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}