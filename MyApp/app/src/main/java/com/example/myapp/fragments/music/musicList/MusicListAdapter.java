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
        initialiseVisibleLayout(view, position);
        initialiseHiddenLayout(view, song);
        initialiseNameView(view, song);
        initialiseLengthView(view, song);
        initialiseDeleteButton(view, song);
    }

    public void initialiseVisibleLayout(View view, int position){
        LinearLayout layoutVisible = view.findViewById(R.id.layoutVisible);
        layoutVisible.setOnClickListener(v -> musicListViewModel.getMusicPlayer().setPlaylist(songList, position));
        layoutVisible.setOnLongClickListener(v -> {
            Song song = songList.get(position);
            buttonMap.put(song, Boolean.FALSE.equals(buttonMap.get(song)));
            notifyDataSetChanged();
            return true;
        });
    }

    public void initialiseHiddenLayout(View view, Song song){
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
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(currentItemView.getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> musicListViewModel.deleteFile(song))
                .setNegativeButton("No", null)
                .create()
                .show());
    }

    public void updateSongList(List<Song> newSongList, String data, String order){
        songList.clear();
        songList.addAll(newSongList);
        sortSongList(data, order);
    }

    public void sortSongList(String data, String order){
        songList.sort(getComparator(data, order));
        for(Song song : songList) buttonMap.put(song, false);
        notifyDataSetChanged();
    }

    public List<Song> getSongList() {
        return songList;
    }

    public Comparator<Song> getComparator(String data, String order){
        Comparator<Song> songComparator = Comparator.comparingInt(Song::getSongID);
        switch (data) {
            case "Date Added":
                songComparator = Comparator.comparingInt(Song::getSongID);
                break;
            case "Name":
                songComparator = Comparator.comparing(Song::getSongName);
                break;
            case "Length":
                songComparator = Comparator.comparing(Song::getSongDuration);
                break;
        }
        return order.equals("Ascending") ? songComparator : songComparator.reversed();
    }
}
