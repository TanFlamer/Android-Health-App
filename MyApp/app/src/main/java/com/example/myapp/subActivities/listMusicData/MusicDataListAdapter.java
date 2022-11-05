package com.example.myapp.subActivities.listMusicData;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;

import java.util.List;

public class MusicDataListAdapter extends ArrayAdapter<MusicDataListItem> {

    public MusicDataListAdapter(@NonNull Context context, int resource, List<MusicDataListItem> musicDataListItemList) {
        super(context, resource, musicDataListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.data_music_list_item, parent, false);

        MusicDataListItem musicDataListItem = getItem(position);

        TextView nameView = currentItemView.findViewById(R.id.musicName);
        TextView lengthView = currentItemView.findViewById(R.id.musicLength);

        nameView.setText(musicDataListItem.getName());
        lengthView.setText(String.valueOf(musicDataListItem.getLength()));

        return currentItemView;
    }
}
