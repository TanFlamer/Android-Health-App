package com.example.myapp.fragments.music.musicStatistics;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;

public class MusicStatisticsFragment extends Fragment {

    MusicStatisticsViewModel musicStatisticsViewModel;
    TextView songTotal, songNumber, songAverage, songLongest, songShortest;
    TextView playlistNumber, playlistLength, playlistCount, playlistLongest, playlistShortest, playlistHighest, playlistLowest;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //get view model
        musicStatisticsViewModel = new ViewModelProvider(this).get(MusicStatisticsViewModel.class);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_statistics, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //get ID for song statistics text views
        initialiseSongViews();
        //get ID for playlist statistics text views
        initialisePlaylistViews();
        //observe live data for playlists and song list
        initialiseLiveData();
    }

    //get ID for song statistics text views
    public void initialiseSongViews(){
        songTotal = requireView().findViewById(R.id.songTotal);
        songNumber = requireView().findViewById(R.id.songNumber);
        songAverage = requireView().findViewById(R.id.songAverage);
        songLongest = requireView().findViewById(R.id.songLongest);
        songShortest = requireView().findViewById(R.id.songShortest);
    }

    //get ID for playlist statistics text views
    public void initialisePlaylistViews(){
        playlistNumber = requireView().findViewById(R.id.playlistNumber);
        playlistLength = requireView().findViewById(R.id.playlistLength);
        playlistCount = requireView().findViewById(R.id.playlistCount);
        playlistLongest = requireView().findViewById(R.id.playlistLongest);
        playlistShortest = requireView().findViewById(R.id.playlistShortest);
        playlistHighest = requireView().findViewById(R.id.playlistHighest);
        playlistLowest = requireView().findViewById(R.id.playlistLowest);
    }

    //observe live data for song list and song catalogue list
    public void initialiseLiveData(){
        //observe live data for song list and update statistics
        musicStatisticsViewModel.getSongLiveData().observe(getViewLifecycleOwner(), this::updateSongResults);
        //observe live data merger of song list and song catalogue list and update statistics
        musicStatisticsViewModel.getMusicDateMerger().observe(getViewLifecycleOwner(), this::updatePlaylistResults);
    }

    //update song statistics if song list changes
    @SuppressLint("DefaultLocale")
    public void updateSongResults(String[] songResults){
        songTotal.setText(songResults[0]);
        songNumber.setText(songResults[1]);
        songAverage.setText(songResults[2]);
        songLongest.setText(songResults[3]);
        songShortest.setText(songResults[4]);
    }

    //update playlists statistics if song list or song catalogue list changes
    public void updatePlaylistResults(String[] playlistResults){
        playlistNumber.setText(playlistResults[0]);
        playlistLength.setText(playlistResults[1]);
        playlistCount.setText(playlistResults[2]);
        playlistLongest.setText(playlistResults[3]);
        playlistShortest.setText(playlistResults[4]);
        playlistHighest.setText(playlistResults[5]);
        playlistLowest.setText(playlistResults[6]);
    }
}