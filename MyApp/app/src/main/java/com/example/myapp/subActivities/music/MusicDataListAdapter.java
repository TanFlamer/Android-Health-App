package com.example.myapp.subActivities.music;

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
import com.example.myapp.databasefiles.song.Song;

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

        initialiseAll(currentItemView, position);
        return currentItemView;
    }

    public void initialiseAll(View view, int position){
        Pair<Song, Boolean> pair = getItem(position);
        initialiseTextView(view, pair.first);
        initialiseSelected(view, pair.second);
    }

    public void initialiseTextView(View view, Song song){
        TextView nameView = view.findViewById(R.id.musicName);
        nameView.setText(song.getSongName());
    }

    public void initialiseSelected(View view, boolean selected){
        view.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);
    }

}
