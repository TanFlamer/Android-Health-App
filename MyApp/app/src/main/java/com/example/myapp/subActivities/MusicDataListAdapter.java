package com.example.myapp.subActivities;

import android.content.Context;
import android.graphics.Color;
import android.util.Pair;
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

public class MusicDataListAdapter extends ArrayAdapter<Pair<Song, Boolean>> {

    public MusicDataListAdapter(@NonNull Context context, int resource, List<Pair<Song, Boolean>> songList) {
        super(context, resource, songList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.data_music_list_item, parent, false);

        Pair<Song, Boolean> songBooleanPair = getItem(position);

        Song song = songBooleanPair.first;

        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);

        nameView.setText(song.getSongName());
        lengthView.setText(String.valueOf(song.getSongDuration()));

        Boolean selected = songBooleanPair.second;
        currentItemView.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);

        return currentItemView;
    }

}
