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
        expandableListView = requireView().findViewById(R.id.musicExpandableListView);
        musicPlaylistsAdapter = new MusicPlaylistsAdapter(requireContext(), new HashMap<>(), musicPlaylistsViewModel);
        expandableListView.setAdapter(musicPlaylistsAdapter);
        expandableListView.setOnItemLongClickListener(onItemLongClickListener);
        musicPlaylistsViewModel.getMusicDateMerger().observe(getViewLifecycleOwner(), songCatalogueHashMap -> musicPlaylistsAdapter.updateMusicPlaylists(songCatalogueHashMap, data, order));
    }

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
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), MusicDataActivity.class)));
    }

    AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            musicPlaylistsAdapter.onLongClick(position);
            return true;
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            collapseAllGroups();
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            musicPlaylistsAdapter.sortMusicPlaylists(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    public void collapseAllGroups(){
        int count = musicPlaylistsAdapter.getGroupCount();
        for(int i = 0; i < count; i++) expandableListView.collapseGroup(i);
    }
}