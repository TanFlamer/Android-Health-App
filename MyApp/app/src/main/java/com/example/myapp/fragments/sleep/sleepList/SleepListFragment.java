package com.example.myapp.fragments.sleep.sleepList;

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
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;

public class SleepListFragment extends Fragment {

    SleepListViewModel sleepListViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    RecyclerView recyclerView;
    SleepListAdapter sleepListAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        sleepListViewModel = new ViewModelProvider(this).get(SleepListViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        //initialise all components
        initialiseAll();
    }

    public void initialiseAll(){
        //initialise sort spinners
        initialiseSpinners();
        //initialise sleep data recycler view
        initialiseRecyclerView();
        //initialise floating button
        initialiseFloatingButton();
    }

    //initialise sleep data recycler view
    public void initialiseRecyclerView(){
        //get recycler view by ID
        recyclerView = requireView().findViewById(R.id.sleepRecyclerView);
        //initialise recycler adapter
        sleepListAdapter = new SleepListAdapter(requireContext(), new ArrayList<>(), sleepListViewModel);
        //set recycler view adapter
        recyclerView.setAdapter(sleepListAdapter);
        //set recycler view fixed size
        recyclerView.setHasFixedSize(true);
        //set recycler view layout manager
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        //observe and reset sleep data list when sleep data changes
        sleepListViewModel.getSleepList().observe(getViewLifecycleOwner(), songList -> {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update sleep data list in adapter
            sleepListAdapter.updateSleepList(songList, data, order);
        });
    }

    //initialise sort spinners
    public void initialiseSpinners(){
        //spinner sort choices
        String[] data = new String[] {"Sleep Date", "Sleep Time", "Wake Time", "Sleep Duration"};
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
            //sort sleep data list
            sleepListAdapter.sortSleepList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    //initialise floating button
    public void initialiseFloatingButton(){
        //get floating button by ID
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        //go to add sleep data activity
        floatingActionButton.setOnClickListener(view1 -> startActivity(sleepListViewModel.sleepAdd()));
    }
}