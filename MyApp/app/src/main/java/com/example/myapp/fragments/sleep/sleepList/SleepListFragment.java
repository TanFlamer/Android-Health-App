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
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseRecyclerView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseRecyclerView(){
        recyclerView = requireView().findViewById(R.id.sleepRecyclerView);
        sleepListAdapter = new SleepListAdapter(requireContext(), new ArrayList<>(), sleepListViewModel);
        recyclerView.setAdapter(sleepListAdapter);
        recyclerView.setHasFixedSize(true);
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));
        sleepListViewModel.getSleepList().observe(getViewLifecycleOwner(), songList -> {
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            sleepListAdapter.updateSleepList(songList, data, order);
        });
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Sleep Date", "Sleep Time", "Wake Time", "Sleep Duration"};
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
            sleepListViewModel.updateSaveLogs("Sleep sorted by " + data + " in " + order + " order");
            sleepListAdapter.sortSleepList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    public void initialiseFloatingButton(){
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> startActivity(sleepListViewModel.sleepAdd()));
    }
}