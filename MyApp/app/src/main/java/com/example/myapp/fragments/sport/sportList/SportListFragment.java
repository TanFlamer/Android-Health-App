package com.example.myapp.fragments.sport.sportList;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ExpandableListView;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.subActivities.sport.SportDataActivity;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.HashMap;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportListFragment#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportListFragment extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportListFragment() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportList.
     */
    // TODO: Rename and change types and number of parameters
    public static SportListFragment newInstance(String param1, String param2) {
        SportListFragment fragment = new SportListFragment();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    SportListViewModel sportListViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ExpandableListView expandableListView;
    SportListAdapter sportListAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        initialiseListView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        expandableListView = requireView().findViewById(R.id.sportExpandableListView);
        expandableListView.setOnItemClickListener(onItemClickListener);

        sportListAdapter = new SportListAdapter(requireContext(), new HashMap<>(), this);
        expandableListView.setAdapter(sportListAdapter);
        sportListViewModel.getSportDataMerger().observe(getViewLifecycleOwner(), sportListHashMap -> sportListAdapter.updateSportList(sportListHashMap, dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString()));
        setListeners(sportListAdapter);
    }

    public void setListeners(SportListAdapter sportListAdapter){
        expandableListView.setOnGroupExpandListener(new ExpandableListView.OnGroupExpandListener() {
            int lastExpandedPosition = -1;
            @Override
            public void onGroupExpand(int i) {
                if(lastExpandedPosition != -1 && i != lastExpandedPosition){
                    expandableListView.collapseGroup(lastExpandedPosition);
                }
                lastExpandedPosition = i;
            }
        });

        expandableListView.setOnChildClickListener((expandableListView1, view1, i, i1, l) -> {
            String selected = sportListAdapter.getChild(i, i1).toString();
            Toast.makeText(getContext(), selected, Toast.LENGTH_SHORT).show();
            return true;
        });
    }

    public AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            Type type = (Type) expandableListView.getItemAtPosition(position);
            Toast.makeText(getContext(), type.getTypeName() + " clicked", Toast.LENGTH_SHORT).show();
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            sportListAdapter.sortSportList(dataSpinner.getSelectedItem().toString(), orderSpinner.getSelectedItem().toString());
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {
            Toast.makeText(getContext(), "Item unselected", Toast.LENGTH_SHORT).show();
        }
    };

    public void initialiseSpinners(){
        String[] data = new String[] {"Date Added", "Name", "Length"};
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
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), SportDataActivity.class)));
    }

    public ExpandableListView getExpandableListView() {
        return expandableListView;
    }

    public SportListViewModel getSportListViewModel() {
        return sportListViewModel;
    }
}