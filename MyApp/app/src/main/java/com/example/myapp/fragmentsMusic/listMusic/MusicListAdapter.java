package com.example.myapp.fragmentsMusic.listMusic;

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
import com.example.myapp.mainActivities.Music;
import com.example.myapp.subActivities.DataMusic;
import com.example.myapp.subActivities.DataSport;

import java.util.List;

public class MusicListAdapter extends ArrayAdapter<MusicListItem> {

    public MusicListAdapter(@NonNull Context context, int resource, List<MusicListItem> musicListItemList) {
        super(context, resource, musicListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.music_list_item, parent, false);

        MusicListItem musicListItem = getItem(position);

        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);

        nameView.setText(musicListItem.getName());
        lengthView.setText(String.valueOf(musicListItem.getLength()));

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
