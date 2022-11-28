package com.example.myapp.fragments.sport.sportStatistics;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.type.Type;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportStatistics#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportStatistics extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportStatistics() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportStatistics.
     */
    // TODO: Rename and change types and number of parameters
    public static SportStatistics newInstance(String param1, String param2) {
        SportStatistics fragment = new SportStatistics();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SportStatisticsViewModel sportStatisticsViewModel;
    Spinner dataSpinner, orderSpinner;
    RecyclerView recyclerView;
    SportRecyclerAdapter sportRecyclerAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        sportRecyclerAdapter = new SportRecyclerAdapter(requireContext(), new HashMap<>());
        recyclerView.setAdapter(sportRecyclerAdapter);
        recyclerView.setHasFixedSize(true);
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        sportStatisticsViewModel.getSportDateMerger().observe(getViewLifecycleOwner(), typeHashMap -> sportRecyclerAdapter.updateSportList(typeHashMap, dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString()));
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
            sportRecyclerAdapter.sortSportList(dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString());
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}