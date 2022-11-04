package com.example.myapp.mainActivities.listSave;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;
import com.example.myapp.fragmentsSport.listSport.SportListItem;

import java.util.List;

public class SaveListAdapter extends ArrayAdapter<SaveListItem> {

    public SaveListAdapter(@NonNull Context context, int resource, List<SaveListItem> saveListItemList) {
        super(context, resource, saveListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.save_list_item, parent, false);

        SaveListItem saveListItem = getItem(position);

        TextView logView = currentItemView.findViewById(R.id.saveLog);
        TextView timeView = currentItemView.findViewById(R.id.saveTime);

        logView.setText(saveListItem.getLog());
        timeView.setText(String.valueOf(saveListItem.getTime()));

        return currentItemView;
    }
}
