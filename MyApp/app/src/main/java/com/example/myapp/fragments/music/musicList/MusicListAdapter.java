package com.example.myapp.fragments.music.musicList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.song.Song;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class MusicListAdapter extends ArrayAdapter<Song> {

    private List<Song> songList;

    public MusicListAdapter(@NonNull Context context, int resource, List<Song> songList) {
        super(context, resource, songList);
        this.songList = songList;
    }

    @SuppressLint("SetTextI18n")
    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.music_list_item, parent, false);
        }

        Song song = songList.get(position);
        initialiseData(currentItemView, song);
        initialiseDeleteButton(currentItemView);
        return currentItemView;
    }

    public void initialiseData(View currentItemView, Song song){
        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);
        nameView.setText(song.getSongName());
        lengthView.setText(String.valueOf(song.getSongDuration()));
    }

    public void initialiseDeleteButton(View currentItemView){
        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(currentItemView.getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", null)
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
