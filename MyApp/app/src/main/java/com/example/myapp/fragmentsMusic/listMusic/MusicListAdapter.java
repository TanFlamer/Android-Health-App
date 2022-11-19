package com.example.myapp.fragmentsMusic.listMusic;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.mainActivities.Music;
import com.example.myapp.subActivities.DataMusic;
import com.example.myapp.subActivities.DataSport;

import java.time.Duration;
import java.util.List;

public class MusicListAdapter extends ArrayAdapter<Song> {

    public MusicListAdapter(@NonNull Context context, int resource, List<Song> songList) {
        super(context, resource, songList);
    }

    @SuppressLint("SetTextI18n")
    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.music_list_item, parent, false);

        Song song = getItem(position);

        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);

        Duration duration = song.getSongDuration();

        nameView.setText(song.getSongName());
        lengthView.setText(duration.toString());

        ImageView clickDelete = currentItemView.findViewById(R.id.clickDelete);
        clickDelete.setOnClickListener(view -> new AlertDialog.Builder(getContext())
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", null)
                .setNegativeButton("No", null)
                .create()
                .show());

        return currentItemView;
    }
}
