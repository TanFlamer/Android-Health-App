package com.example.myapp.fragments.sport.sportType;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Spinner;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.subActivities.type.TypeDataActivity;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;

public class SportTypeFragment extends Fragment {

    SportTypeViewModel sportTypeViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ListView listView;
    SportTypeAdapter sportTypeAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        sportTypeViewModel = new ViewModelProvider(this).get(SportTypeViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_type, container, false);
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
        //initialise sport type list view
        initialiseListView();
        //initialise floating button
        initialiseFloatingButton();
    }

    //initialise sport type list view
    public void initialiseListView(){
        //get list view by ID
        listView = requireView().findViewById(R.id.sportListView);
        //initialise list adapter
        sportTypeAdapter = new SportTypeAdapter(requireContext(), 0, new ArrayList<>(), sportTypeViewModel);
        //set list view adapter
        listView.setAdapter(sportTypeAdapter);
        //set list view on item long click listener
        listView.setOnItemLongClickListener(onItemLongClickListener);
        //observe and reset sport type list when sport type list changes
        sportTypeViewModel.getTypeList().observe(getViewLifecycleOwner(), typeList -> {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update sport type list in adapter
            sportTypeAdapter.updateTypeList(typeList, data, order);
        });
    }

    //initialise sort spinners
    public void initialiseSpinners(){
        //spinner sort choices
        String[] data = new String[] {"Date Added", "Name", "Calorie"};
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
        //go to add sport type activity on click
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), TypeDataActivity.class)));
    }

    //on item long click listener for list view
    public AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            //show or hide additional buttons
            sportTypeAdapter.onLongClick(position);
            return true;
        }
    };

    //on item selected listener for spinners
    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //sort sport type list
            sportTypeAdapter.sortTypeList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}