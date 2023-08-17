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
import com.example.myapp.databaseFiles.song.Song;

import java.util.List;

public class MusicDataListAdapter extends ArrayAdapter<Pair<Song, Boolean>> {

    //constructor for song list adapter
    public MusicDataListAdapter(@NonNull Context context, int resource, List<Pair<Song, Boolean>> songList) {
        super(context, resource, songList);
    }

    @NonNull
    @Override //get view for each song
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for song if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.data_music_list_item, parent, false);
        }

        //initialise song view data
        initialiseAll(currentItemView, position);
        //return song view
        return currentItemView;
    }

    //initialise song view data
    public void initialiseAll(View view, int position){
        Pair<Song, Boolean> pair = getItem(position);
        //get song name
        initialiseTextView(view, pair.first);
        //get song selected state
        initialiseSelected(view, pair.second);
    }

    //get song name
    public void initialiseTextView(View view, Song song){
        //get text view ID for song name
        TextView nameView = view.findViewById(R.id.musicName);
        //set song name
        nameView.setText(song.getSongName());
    }

    //get song selected state
    public void initialiseSelected(View view, boolean selected){
        //set song background colour depending on selected state
        view.setBackgroundColor(selected ? Color.LTGRAY : Color.WHITE);
    }

}
