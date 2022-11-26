package com.example.myapp.fragments.music.musicPlaylists;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ExpandableListView;
import android.widget.Spinner;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.subActivities.music.DataMusic;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.HashMap;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link MusicPlaylists#newInstance} factory method to
 * create an instance of this fragment.
 */
public class MusicPlaylists extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public MusicPlaylists() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment Playlists.
     */
    // TODO: Rename and change types and number of parameters
    public static MusicPlaylists newInstance(String param1, String param2) {
        MusicPlaylists fragment = new MusicPlaylists();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    MusicPlaylistsViewModel musicPlaylistsViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ExpandableListView expandableListView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        initialiseListView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        expandableListView = requireView().findViewById(R.id.musicExpandableListView);
        expandableListView.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        expandableListView.setOnItemClickListener(onItemClickListener);
        expandableListView.setOnItemLongClickListener(onItemLongClickListener);
        expandableListView.setOnItemSelectedListener(onItemSelectedListener);

        MusicExpandableListAdapter musicExpandableListAdapter = new MusicExpandableListAdapter(requireContext(), new HashMap<>());
        expandableListView.setAdapter(musicExpandableListAdapter);
        musicPlaylistsViewModel.getSongPlaylistList().observe(getViewLifecycleOwner(), songPlaylists -> musicExpandableListAdapter.updateMusicPlaylists(musicPlaylistsViewModel.updateMusicPlaylists(songPlaylists)));
        setListeners(musicExpandableListAdapter);
    }

    public void setListeners(MusicExpandableListAdapter musicExpandableListAdapter){
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
            String selected = musicExpandableListAdapter.getChild(i, i1).toString();
            Toast.makeText(getContext(), selected, Toast.LENGTH_SHORT).show();
            return true;
        });
    }

    public AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) expandableListView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " clicked", Toast.LENGTH_SHORT).show();
        }
    };

    public AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) expandableListView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " long clicked", Toast.LENGTH_SHORT).show();
            return true;
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) expandableListView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " selected", Toast.LENGTH_SHORT).show();
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {
            Toast.makeText(getContext(), "Item unselected", Toast.LENGTH_SHORT).show();
        }
    };

    public void initialiseSpinners(){
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        orderSpinner = requireView().findViewById(R.id.orderSpinner);
    }

    public void initialiseFloatingButton(){
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> startActivity(new Intent(getContext(), DataMusic.class)));
    }
}