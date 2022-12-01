package com.example.myapp.fragments.music.musicStatistics;

import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.song.Song;

import java.util.List;

public class MusicStatisticsFragment extends Fragment {

    MusicStatisticsViewModel musicStatisticsViewModel;
    TextView songTotal, songNumber, songAverage, songLongest, songShortest;
    TextView playlistNumber, playlistLength, playlistCount, playlistLongest, playlistShortest, playlistHighest, playlistLowest;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
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
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseSongs();
        initialisePlaylists();
        initialiseLiveData();
    }

    public void initialiseSongs(){
        songTotal = requireView().findViewById(R.id.songTotal);
        songNumber = requireView().findViewById(R.id.songNumber);
        songAverage = requireView().findViewById(R.id.songAverage);
        songLongest = requireView().findViewById(R.id.songLongest);
        songShortest = requireView().findViewById(R.id.songShortest);
    }

    public void initialisePlaylists(){
        playlistNumber = requireView().findViewById(R.id.playlistNumber);
        playlistLength = requireView().findViewById(R.id.playlistLength);
        playlistCount = requireView().findViewById(R.id.playlistCount);
        playlistLongest = requireView().findViewById(R.id.playlistLongest);
        playlistShortest = requireView().findViewById(R.id.playlistShortest);
        playlistHighest = requireView().findViewById(R.id.playlistHighest);
        playlistLowest = requireView().findViewById(R.id.playlistLowest);
    }

    public void initialiseLiveData(){
        musicStatisticsViewModel.getSongLiveData().observe(getViewLifecycleOwner(), this::updateSongResults);
        musicStatisticsViewModel.getMusicDateMerger().observe(getViewLifecycleOwner(), this::updatePlaylistResults);
    }

    public void updateSongResults(double[] songResults){
        songTotal.setText(String.valueOf(songResults[0]));
        songNumber.setText(String.valueOf(songResults[1]));
        songAverage.setText(String.valueOf(songResults[2]));
        songLongest.setText(String.valueOf(songResults[3]));
        songShortest.setText(String.valueOf(songResults[4]));
    }

    public void updatePlaylistResults(double[] playlistResults){
        playlistNumber.setText(String.valueOf(playlistResults[0]));
        playlistLength.setText(String.valueOf(playlistResults[1]));
        playlistCount.setText(String.valueOf(playlistResults[2]));
        playlistLongest.setText(String.valueOf(playlistResults[3]));
        playlistShortest.setText(String.valueOf(playlistResults[4]));
        playlistHighest.setText(String.valueOf(playlistResults[5]));
        playlistLowest.setText(String.valueOf(playlistResults[6]));
    }
}