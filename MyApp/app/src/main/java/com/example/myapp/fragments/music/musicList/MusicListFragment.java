package com.example.myapp.fragments.music.musicList;

import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.io.IOException;
import java.util.ArrayList;

public class MusicListFragment extends Fragment {

    MusicListViewModel musicListViewModel;
    MusicListAdapter musicListAdapter;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ListView listView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        musicListViewModel = new ViewModelProvider(this).get(MusicListViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_list, container, false);
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
        //get list view by ID
        listView = requireView().findViewById(R.id.musicListView);
        //initialise list adapter
        musicListAdapter = new MusicListAdapter(requireContext(), 0, new ArrayList<>(), musicListViewModel);
        //set list view adapter
        listView.setAdapter(musicListAdapter);
        //set list view on item click listener
        listView.setOnItemClickListener(onItemClickListener);
        //set list view on item long click listener
        listView.setOnItemLongClickListener(onItemLongClickListener);
        //observe and reset song list when song list changes
        musicListViewModel.getSongList().observe(getViewLifecycleOwner(), songList -> {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //update song list in adapter
            musicListAdapter.updateSongList(songList, data, order);
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
        //ask for read media files permission and get music files from device on click
        floatingActionButton.setOnClickListener(view1 -> musicListViewModel.getMusicFile(mGetContent, requestPermissionLauncher));
    }

    //on item click listener for list view
    AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            //get music player
            MusicPlayer musicPlayer = musicListViewModel.getMusicPlayer();
            //play clicked song
            musicListAdapter.onClick(musicPlayer, position);
        }
    };

    //on item long click listener for list view
    AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            //show or hide additional buttons
            musicListAdapter.onLongClick(position);
            return true;
        }
    };

    //on item selected listener for spinners
    AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            //get sort data
            String data = dataSpinner.getSelectedItem().toString();
            //get sort order
            String order = orderSpinner.getSelectedItem().toString();
            //sort song list
            musicListAdapter.sortSongList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    //get music files from device
    private final ActivityResultLauncher<String> mGetContent = registerForActivityResult(new ActivityResultContracts.GetMultipleContents(),
            uri -> {
                if(uri.size() != 0){ //if any music file selected
                    for(int i = 0; i < uri.size(); i++){
                        Uri currentUri = uri.get(i); //loop through uri
                        try {
                            musicListViewModel.copyFile(currentUri, requireActivity()); //copy music file to music folder
                        }
                        catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
                else //show toast if no music file selected
                    Toast.makeText(getContext(), "No file selected", Toast.LENGTH_SHORT).show();
            });

    //ask user to give read media files permission
    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) { //if permission granted
                    mGetContent.launch("audio/*"); //get music files from device
                }
                else { //else show toast
                    Toast.makeText(getContext(), "Permission not granted to copy files", Toast.LENGTH_SHORT).show();
                }
            });
}