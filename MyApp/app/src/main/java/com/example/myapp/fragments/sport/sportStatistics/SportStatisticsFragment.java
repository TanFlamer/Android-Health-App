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
        //get view model
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
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //initialise sort spinners
        initialiseSpinners();
        //initialise sport statistics recycler view
        initialiseRecyclerView();
    }

    //initialise sport statistics recycler view
    public void initialiseRecyclerView(){
        //get recycler view by ID
        recyclerView = requireView().findViewById(R.id.sportRecyclerView);
        //initialise recycler adapter
        sportStatisticsAdapter = new SportStatisticsAdapter(requireContext(), new HashMap<>(), sportStatisticsViewModel);
        //set recycler view adapter
        recyclerView.setAdapter(sportStatisticsAdapter);
        //set recycler view fixed size
        recyclerView.setHasFixedSize(true);
        //set recycler view layout manager
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        //observe and reset sport statistics list when sport data changes
        sportStatisticsViewModel.getSportDateMerger().observe(getViewLifecycleOwner(), typeHashMap -> {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update sport statistics list in adapter
            sportStatisticsAdapter.updateSportList(typeHashMap, data, order);
        });
    }

    //initialise sort spinners
    public void initialiseSpinners(){
        //spinner sort choices
        String[] data = new String[] {"Date Added", "Name", "Total Days", "Total Duration", "Total Calorie", "Average Duration", "Average Calorie", "Max Duration", "Max Calorie", "Min Duration", "Min Calorie"};
        String[] order = new String[] {"Ascending", "Descending"};
        //get spinners by ID
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        orderSpinner = requireView().findViewById(R.id.orderSpinner);
        //set spinners with adapters
        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        orderSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, order));
        //set on item selected listener to spinners
        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
        orderSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    //on item selected listener for spinners
    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //sort sport statistics list
            sportStatisticsAdapter.sortSportList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}