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
        //get view model
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
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //initialise sort spinners
        initialiseSpinners();
        //initialise sport data list view
        initialiseListView();
        //initialise floating button
        initialiseFloatingButton();
    }

    //initialise sport data list view
    public void initialiseListView(){
        //get expandable list view by ID
        expandableListView = requireView().findViewById(R.id.sportExpandableListView);
        //initialise list adapter
        sportListAdapter = new SportListAdapter(requireContext(), new HashMap<>(), sportListViewModel);
        //set list view adapter
        expandableListView.setAdapter(sportListAdapter);
        //set list view on item long click listener
        expandableListView.setOnItemLongClickListener(onItemLongClickListener);
        //observe and reset sport list when sport data list changes
        sportListViewModel.getSportDataMerger().observe(getViewLifecycleOwner(), sportListHashMap -> {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update sport list in adapter
            sportListAdapter.updateSportList(sportListHashMap, data, order);
        });
    }

    //initialise sort spinners
    public void initialiseSpinners(){
        //spinner sort choices
        String[] data = new String[] {"Sport Date", "Name", "Calories", "Duration"};
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

    //initialise floating button
    public void initialiseFloatingButton(){
        //get floating button by ID
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        //go to add sport data activity
        floatingActionButton.setOnClickListener(view1 -> startActivity(sportListViewModel.sportAdd()));
    }

    //on item long click listener for list view
    AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            //show or hide additional buttons
            sportListAdapter.onLongClick(position);
            return true;
        }
    };

    //on item selected listener for spinners
    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            //collapse all expanded playlists
            collapseAllGroups();
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //sort sport list
            sportListAdapter.sortSportList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    //collapse all expanded sport data
    public void collapseAllGroups(){
        int count = sportListAdapter.getGroupCount();
        for(int i = 0; i < count; i++) expandableListView.collapseGroup(i);
    }
}