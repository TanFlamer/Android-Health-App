package com.example.myapp.fragments.music.musicList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databasefiles.song.Song;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class MusicListAdapter extends ArrayAdapter<Song> {

    private final List<Song> songList;
    private final HashMap<Song, Boolean> buttonMap;
    private final MusicListViewModel musicListViewModel;

    public MusicListAdapter(@NonNull Context context, int resource, List<Song> songList, MusicListViewModel musicListViewModel) {
        super(context, resource, songList);
        this.songList = songList;
        this.musicListViewModel = musicListViewModel;
        buttonMap = new HashMap<>();
        for(Song song : songList) buttonMap.put(song, false);
    }

    @SuppressLint("SetTextI18n")
    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.music_list_item, parent, false);
        }

        initialiseAll(currentItemView, position);
        return currentItemView;
    }

    public void initialiseAll(View view, int position){
        Song song = songList.get(position);
        initialiseLayouts(view, song);
        initialiseNameView(view, song);
        initialiseLengthView(view, song);
        initialiseDeleteButton(view, song);
    }

    public void onClick(MusicPlayer musicPlayer, int position){
        musicPlayer.setPlaylist(songList, position);
    }

    public void onLongClick(int position){
        Song song = songList.get(position);
        buttonMap.put(song, Boolean.FALSE.equals(buttonMap.get(song)));
        notifyDataSetChanged();
    }

    public void initialiseLayouts(View view, Song song){
        LinearLayout layoutHidden = view.findViewById(R.id.layoutHidden);
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(song)) ? View.VISIBLE : View.GONE);
    }

    public void initialiseNameView(View view, Song song){
        TextView nameView = view.findViewById(R.id.musicName);
        nameView.setText(song.getSongName());
    }

    public void initialiseLengthView(View view, Song song){
        TextView lengthView = view.findViewById(R.id.musicLength);
        lengthView.setText(String.valueOf(song.getSongDuration()));
    }

    public void initialiseDeleteButton(View currentItemView, Song song){
        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> musicListViewModel.deleteSong(currentItemView.getContext(), song).show());
    }

    public void updateSongList(List<Song> newSongList, String data, String order){
        songList.clear();
        songList.addAll(newSongList);
        sortSongList(data, order);
    }

    public void sortSongList(String data, String order){
        musicListViewModel.sortSongList(songList, data, order);
        for(Song song : songList) buttonMap.put(song, false);
        notifyDataSetChanged();
    }
}
