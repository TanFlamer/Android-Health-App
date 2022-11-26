package com.example.myapp.fragments.music.musicStatistics;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.example.myapp.R;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link MusicStatistics#newInstance} factory method to
 * create an instance of this fragment.
 */
public class MusicStatistics extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public MusicStatistics() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment statisticsMP3.
     */
    // TODO: Rename and change types and number of parameters
    public static MusicStatistics newInstance(String param1, String param2) {
        MusicStatistics fragment = new MusicStatistics();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    MusicStatisticsViewModel musicStatisticsViewModel;
    TextView songTotal, songNumber, songAverage, songLongest, songShortest;
    TextView playlistNumber, playlistLength, playlistCount, playlistLongest, playlistShortest, playlistHighest, playlistLowest;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
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
        initialiseViews();
        musicStatisticsViewModel.getSongDateMerger().observe(getViewLifecycleOwner(), this::updateResults);
    }

    public void initialiseViews(){
        initialiseSongs();
        initialisePlaylists();
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

    public void updateResults(Pair<int[], int[]> pair){
        int[] songResults = pair.first;
        songTotal.setText(String.valueOf(songResults[0]));
        songNumber.setText(String.valueOf(songResults[1]));
        songAverage.setText(String.valueOf(songResults[0] / (double) songResults[1]));
        songLongest.setText(String.valueOf(songResults[2]));
        songShortest.setText(String.valueOf(songResults[3]));

        int[] playlistResults = pair.second;
        playlistNumber.setText(String.valueOf(playlistResults[0]));
        playlistLength.setText(String.valueOf(playlistResults[3] / (double) playlistResults[0]));
        playlistCount.setText(String.valueOf(playlistResults[6] / (double) playlistResults[0]));
        playlistLongest.setText(String.valueOf(playlistResults[4]));
        playlistShortest.setText(String.valueOf(playlistResults[5]));
        playlistHighest.setText(String.valueOf(playlistResults[1]));
        playlistLowest.setText(String.valueOf(playlistResults[2]));
    }
}