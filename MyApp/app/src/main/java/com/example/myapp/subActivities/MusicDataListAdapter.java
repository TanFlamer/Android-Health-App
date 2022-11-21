package com.example.myapp.subActivities;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Song;

import java.util.List;

public class MusicDataListAdapter extends ArrayAdapter<Song> {

    private List<Song> songList;

    public MusicDataListAdapter(@NonNull Context context, int resource, List<Song> songList) {
        super(context, resource, songList);
        this.songList = songList;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.data_music_list_item, parent, false);

        Song song = getItem(position);

        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);

        nameView.setText(song.getSongName());
        lengthView.setText(String.valueOf(song.getSongDuration()));

        return currentItemView;
    }

    public void updateSongList(List<Song> newSongList){
        songList.clear();
        songList.addAll(newSongList);
        notifyDataSetChanged();
    }
}