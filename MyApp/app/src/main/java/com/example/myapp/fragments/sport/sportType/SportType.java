package com.example.myapp.fragments.sport.sportType;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
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
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.subActivities.type.DataType;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportType#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportType extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportType() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportType.
     */
    // TODO: Rename and change types and number of parameters
    public static SportType newInstance(String param1, String param2) {
        SportType fragment = new SportType();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SportTypeViewModel sportTypeViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ListView listView;
    SportListAdapter sportListAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        listView.setOnItemClickListener(onItemClickListener);

        sportListAdapter = new SportListAdapter(requireContext(), 0, new ArrayList<>());
        listView.setAdapter(sportListAdapter);
        sportTypeViewModel.getTypeList().observe(getViewLifecycleOwner(), typeList -> sportListAdapter.updateTypeList(typeList, dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString()));
    }

    public AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            Type type = (Type) listView.getItemAtPosition(position);
            Toast.makeText(getContext(), type.getTypeName() + " clicked", Toast.LENGTH_SHORT).show();
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            sportListAdapter.sortTypeList(dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString());
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

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
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), DataType.class)));
    }
}