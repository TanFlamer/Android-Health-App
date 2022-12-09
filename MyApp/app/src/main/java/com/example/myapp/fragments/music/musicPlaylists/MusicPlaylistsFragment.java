package com.example.myapp.fragments.music.musicPlaylists;

import android.content.Intent;
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
import com.example.myapp.subActivities.music.MusicDataActivity;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.HashMap;

public class MusicPlaylistsFragment extends Fragment {

    MusicPlaylistsViewModel musicPlaylistsViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ExpandableListView expandableListView;
    MusicPlaylistsAdapter musicPlaylistsAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        musicPlaylistsViewModel = new ViewModelProvider(this).get(MusicPlaylistsViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_playlists, container, false);
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
        //initialise song list view
        initialiseListView();
        //initialise floating button
        initialiseFloatingButton();
    }

    //initialise song list view
    public void initialiseListView(){
        //get expandable list view by ID
        expandableListView = requireView().findViewById(R.id.musicExpandableListView);
        //initialise list adapter
        musicPlaylistsAdapter = new MusicPlaylistsAdapter(requireContext(), new HashMap<>(), musicPlaylistsViewModel);
        //set list view adapter
        expandableListView.setAdapter(musicPlaylistsAdapter);
        //set list view on item long click listener
        expandableListView.setOnItemLongClickListener(onItemLongClickListener);
        //observe and reset playlists list when playlists list changes
        musicPlaylistsViewModel.getMusicDateMerger().observe(getViewLifecycleOwner(), songCatalogueHashMap -> {
            //collapse all expanded playlists
            collapseAllGroups();
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update playlists list in adapter
            musicPlaylistsAdapter.updateMusicPlaylists(songCatalogueHashMap, data, order);
        });
    }

    //initialise sort spinners
    public void initialiseSpinners(){
        //spinner sort choices
        String[] data = new String[] {"Date Added", "Name", "Length"};
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
        //go to add playlists activity
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), MusicDataActivity.class)));
    }

    //on item long click listener for list view
    AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            //show or hide additional buttons
            musicPlaylistsAdapter.onLongClick(position);
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
            //sort song list
            musicPlaylistsAdapter.sortMusicPlaylists(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    //collapse all expanded playlists
    public void collapseAllGroups(){
        int count = musicPlaylistsAdapter.getGroupCount();
        for(int i = 0; i < count; i++) expandableListView.collapseGroup(i);
    }
}