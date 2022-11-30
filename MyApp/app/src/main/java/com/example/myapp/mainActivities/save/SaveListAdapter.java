package com.example.myapp.mainActivities.save;

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

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class SaveListAdapter extends ArrayAdapter<Pair<String, LocalDateTime>> {

    private List<Pair<String, LocalDateTime>> saveLogs;

    public SaveListAdapter(@NonNull Context context, int resource, List<Pair<String, LocalDateTime>> saveLogs) {
        super(context, resource, saveLogs);
        this.saveLogs = saveLogs;
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.save_list_item, parent, false);

        Pair<String, LocalDateTime> saveLog = getItem(position);

        TextView logView = currentItemView.findViewById(R.id.saveLog);
        TextView timeView = currentItemView.findViewById(R.id.saveTime);

        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String formattedDateTime = saveLog.second.format(dateTimeFormatter);

        logView.setText(saveLog.first);
        timeView.setText(formattedDateTime);

        return currentItemView;
    }

    public void updateSaveLogs(Pair<String, LocalDateTime> newSaveLogs){
        saveLogs.add(newSaveLogs);
        notifyDataSetChanged();
    }
}
