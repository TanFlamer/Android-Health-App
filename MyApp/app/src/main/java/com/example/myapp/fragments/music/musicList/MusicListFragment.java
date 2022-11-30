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
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseListView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        listView = requireView().findViewById(R.id.musicListView);
        musicListAdapter = new MusicListAdapter(requireContext(), 0, new ArrayList<>(), musicListViewModel);
        listView.setAdapter(musicListAdapter);
        musicListViewModel.getSongList().observe(getViewLifecycleOwner(), songList -> {
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            musicListAdapter.updateSongList(songList, data, order);
        });
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
        floatingActionButton.setOnClickListener(view1 -> musicListViewModel.getMusicFile(mGetContent, requestPermissionLauncher));
    }

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            String data = dataSpinner.getSelectedItem().toString();
            String order = orderSpinner.getSelectedItem().toString();
            musicListAdapter.sortSongList(data, order);
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {

        }
    };

    private final ActivityResultLauncher<String> mGetContent = registerForActivityResult(new ActivityResultContracts.GetMultipleContents(),
            uri -> {
                if(uri.size() != 0){
                    for(int i = 0; i < uri.size(); i++){
                        Uri currentUri = uri.get(i);
                        try {
                            musicListViewModel.copyFile(currentUri, requireActivity());
                        }
                        catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
                else
                    Toast.makeText(getContext(), "No file selected", Toast.LENGTH_SHORT).show();
            });

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    mGetContent.launch("audio/*");
                }
                else {
                    Toast.makeText(getContext(), "Permission not granted to copy files", Toast.LENGTH_SHORT).show();
                }
            });
}