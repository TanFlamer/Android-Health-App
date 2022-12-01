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
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;
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
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseListView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        listView = requireView().findViewById(R.id.sportListView);
        sportTypeAdapter = new SportTypeAdapter(requireContext(), 0, new ArrayList<>(), sportTypeViewModel);
        listView.setAdapter(sportTypeAdapter);
        listView.setOnItemLongClickListener(onItemLongClickListener);
        sportTypeViewModel.getTypeList().observe(getViewLifecycleOwner(), typeList -> sportTypeAdapter.updateTypeList(typeList, dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString()));
    }

    public void initialiseSpinners(){
        String[] data = new String[] {"Date Added", "Name", "Calorie"};
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
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), TypeDataActivity.class)));
    }

    public AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {

            return true;
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            sportTypeAdapter.sortTypeList(dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString());
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };
}