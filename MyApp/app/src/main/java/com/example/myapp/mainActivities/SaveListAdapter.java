package com.example.myapp.mainActivities;

import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;

import java.time.LocalTime;
import java.util.List;

public class SaveListAdapter extends ArrayAdapter<Pair<String, LocalTime>> {

    private List<Pair<String, LocalTime>> saveLogs;

    public SaveListAdapter(@NonNull Context context, int resource, List<Pair<String, LocalTime>> saveLogs) {
        super(context, resource, saveLogs);
        this.saveLogs = saveLogs;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.save_list_item, parent, false);

        Pair<String, LocalTime> saveLog = getItem(position);

        TextView logView = currentItemView.findViewById(R.id.saveLog);
        TextView timeView = currentItemView.findViewById(R.id.saveTime);

        logView.setText(saveLog.first);
        timeView.setText(saveLog.second.toString());

        return currentItemView;
    }

    public void updateSaveLogs(List<Pair<String, LocalTime>> newSaveLogs){
        saveLogs.clear();
        saveLogs.addAll(newSaveLogs);
        notifyDataSetChanged();
    }
}
