package com.example.myapp.fragments.sport.sportList;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ExpandableListView;
import android.widget.Spinner;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.HashMap;

public class SportListFragment extends Fragment {

    SportListViewModel sportListViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ExpandableListView expandableListView;
    SportListAdapter sportListAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sportListViewModel = new ViewModelProvider(this).get(SportListViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseSpinners();
        initialiseListView();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        String data = dataSpinner.getSelectedItem().toString();
        String order = orderSpinner.getSelectedItem().toString();
        expandableListView = requireView().findViewById(R.id.sportExpandableListView);
        sportListAdapter = new SportListAdapter(requireContext(), new HashMap<>(), sportListViewModel);
        expandableListView.setAdapter(sportListAdapter);
        expandableListView.setOnItemLongClickListener(onItemLongClickListener);
        sportListViewModel.getSportDataMerger().observe(getViewLifecycleOwner(), sportListHashMap -> sportListAdapter.updateSportList(sportListHashMap, data, order));
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Date Added", "Sport Date", "Name", "Calories", "Duration"};
        String[] order = new String[] {"Ascending", "Descending"};

        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        orderSpinner = requireView().findViewById(R.id.orderSpinner);

        dataSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, data));
        orderSpinner.setAdapter(new ArrayAdapter<>(getActivity(), android.R.layout.simple_spinner_dropdown_item, order));

        dataSpinner.setOnItemSelectedListener(onItemSelectedListener);
        orderSpinner.setOnItemSelectedListener(onItemSelectedListener);
    }

    public void initialiseFloatingButton(){
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> startActivity(sportListViewModel.sportAdd()));
    }

    public void collapseAllGroups(){
        int count = sportListAdapter.getGroupCount();
        for(int i = 0; i < count; i++) expandableListView.collapseGroup(i);
    }

    AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            sportListAdapter.onLongClick(position);
            return true;
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            collapseAllGroups();
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            sportListAdapter.sortSportList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}